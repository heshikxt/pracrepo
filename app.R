library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(gridExtra)  # プロットとテーブルの統合
library(grid)       # グラフィックレイアウト用

# データ読み込み用の関数
load_datasets <- function() {
  # メタファイルの読み込み
  meta_data <- read_excel("metafile.xlsx")
  
  # 利用可能なデータセットを格納するリスト
  datasets <- list()
  
  # メタファイルの各行についてデータ読み込みを試行
  for(i in 1:nrow(meta_data)) {
    filename <- meta_data$Filename[i]
    
    # ファイルが存在する場合のみ読み込み
    if(file.exists(filename)) {
      tryCatch({
        # タブ区切りテキストファイルとして読み込み
        data <- read_delim(filename, 
                           delim = "\t",
                           col_types = cols(
                             Signature = col_character(),
                             HR.TDXd = col_double(),
                             HR.cl.TDXd = col_double(),
                             HR.cu.TDXd = col_double(),
                             median.PFS.0 = col_character(),
                             median.PFS.1 = col_character(),
                             n.0.TDXd = col_double(),
                             n.1.TDXd = col_double()
                           )) %>%
          mutate(
            # median.PFS.0 の処理
            PFS.0.cl = as.numeric(str_extract(median.PFS.0, "(?<=\\()[0-9]+\\.?[0-9]*")), # B の抽出 (下限値)
            PFS.0.cu = as.numeric(str_extract(median.PFS.0, "(?<=-)[0-9]+\\.?[0-9]*")),   # エラーバー上限値 (12.3)
            median.PFS.0 = as.numeric(str_extract(median.PFS.0, "^[0-9]+\\.?[0-9]*")),      # A の抽出 (PFSの値)
            
            # median.PFS.1 の処理
            PFS.1.cl = as.numeric(str_extract(median.PFS.1, "(?<=\\()[0-9]+\\.?[0-9]*")), # エラーバー下限値 (8.12)
            PFS.1.cu = as.numeric(str_extract(median.PFS.1, "(?<=-)[0-9]+\\.?[0-9]*")),    # エラーバー上限値 (12.3)
            median.PFS.1 = as.numeric(str_extract(median.PFS.1, "^[0-9]+\\.?[0-9]*"))          # PFSの値 (9.8)
          )
        
        # データセットにメタ情報を追加
        data$Project <- meta_data$Project[i]
        data$Study <- meta_data$Study[i]
        data$Arm <- meta_data$Arm[i]
        data$Subgroup <- meta_data$Subgroup[i]
        data$Source <- filename
        datasets[[filename]] <- data
      }, error = function(e) {
        warning(paste("Failed to load:", filename))
      })
    }
  }
  
  return(datasets)
}

# UI定義
ui <- fluidPage(
  titlePanel("Forest Plot Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # メタデータのフィルター
      selectizeInput("project_filter",
                     "Filter by Project",
                     choices = NULL,
                     multiple = TRUE),
      
      selectizeInput("study_filter",
                     "Filter by Study",
                     choices = NULL,
                     multiple = TRUE),
      
      selectizeInput("arm_filter",
                     "Filter by Arm",
                     choices = NULL,
                     multiple = TRUE),
      
      selectizeInput("subgroup_filter",
                     "Filter by Subgroup",
                     choices = NULL,
                     multiple = TRUE),
      
      # Signature選択
      selectizeInput("selected_signatures",
                     "Select Signatures",
                     choices = NULL,
                     multiple = TRUE),
      
      selectInput("selected_metric",
                  "Select Metric to Display",
                  choices = c("HR.TDXd", "median.PFS.0", "median.PFS.1"),
                  selected = "HR.TDXd"),
      
      # プロットオプション
      sliderInput("axis_size", "Axis Font Size:", min = 8, max = 20, value = 12),
      #      checkboxInput("show_ci", "Show Confidence Interval", TRUE),
      checkboxInput("log_scale", "Use Log Scale", TRUE)
    ),
    
    mainPanel(
      plotOutput("forest_plot", height = "800px"),
      downloadButton("download_plot", "Download Plot")
    )
  )
)

# Server定義
server <- function(input, output, session) {
  # データの読み込み
  datasets <- reactiveVal(load_datasets())
  
  # UI更新用の関数
  observe({
    all_data <- datasets()
    if(length(all_data) > 0) {
      # 全データを結合
      combined_data <- bind_rows(all_data)
      
      # フィルター選択肢の更新
      updateSelectizeInput(session, "project_filter",
                           choices = unique(combined_data$Project))
      updateSelectizeInput(session, "study_filter",
                           choices = unique(combined_data$Study))
      updateSelectizeInput(session, "arm_filter",
                           choices = unique(combined_data$Arm))
      updateSelectizeInput(session, "subgroup_filter",
                           choices = unique(combined_data$Subgroup))
      updateSelectizeInput(session, "selected_signatures",
                           choices = unique(combined_data$Signature))
    }
  })
  
  # フィルタリングされたデータの生成
  filtered_data <- reactive({
    req(datasets())
    plot_data <- bind_rows(datasets())
    # フィルター適用
    if(!is.null(input$project_filter))
      plot_data <- plot_data %>% filter(Project %in% input$project_filter)
    if(!is.null(input$study_filter))
      plot_data <- plot_data %>% filter(Study %in% input$study_filter)
    if(!is.null(input$arm_filter))
      plot_data <- plot_data %>% filter(Arm %in% input$arm_filter)
    if(!is.null(input$subgroup_filter))
      plot_data <- plot_data %>% filter(Subgroup %in% input$subgroup_filter)
    if(!is.null(input$selected_signatures))
      plot_data <- plot_data %>% filter(Signature %in% input$selected_signatures)
    
    plot_data
  })
  
  # プロット生成
  generate_plot_and_table <- reactive({
    req(filtered_data())
    plot_data <- filtered_data()
    selected_metric <- input$selected_metric
    
    # ラベル生成
    plot_data <- plot_data %>%
      arrange(Study, Arm, .data[[selected_metric]]) %>%
      mutate(label = paste(Source, Study, Arm, Subgroup, sep = " | "))
    
    p <- ggplot(plot_data, 
                aes(y = reorder(Signature, !!sym(selected_metric)))) +
      #      aes(y = reorder(paste(label, Signature), HR.TDXd))) +
      geom_point(aes(x = !!sym(selected_metric))) +
      geom_vline(xintercept = ifelse(selected_metric == "HR.TDXd", 1, NA), linetype = "dashed") +
      facet_wrap(vars(Study, Arm)) + #, scales = "free_y", space = "free") +
      labs(x = selected_metric, y = "") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = input$axis_size),
        strip.text = element_text(size = input$axis_size),
        axis.text = element_text(size = input$axis_size))
    
    table_data <- plot_data
    if(selected_metric == "HR.TDXd") {
      p <- p + geom_errorbarh(aes(xmin = HR.cl.TDXd, 
                                  xmax = HR.cu.TDXd), 
                              height = 0.2)
      table_data <- plot_data %>%
        select(HR.TDXd) 
    }
    if (selected_metric == "median.PFS.0") {
      p <- p + geom_errorbarh(aes(xmin = PFS.0.cl, 
                                  xmax = PFS.0.cu), 
                              height = 0.2)
      table_data <- plot_data %>%
        select(n.0.TDXd) %>%
        rename("n" = n.0.TDXd)
      
    }
    if (selected_metric == "median.PFS.1") {
      p <- p + geom_errorbarh(aes(xmin = PFS.1.cl, 
                                  xmax = PFS.1.cu), 
                              height = 0.2)
      table_data <- plot_data %>%
        select(n.1.TDXd) %>%
        rename("n" = n.1.TDXd)
    }
    
    if(input$log_scale) {
      p <- p + scale_x_continuous(trans = "log10")
    }
    
    g_table <- tableGrob(table_data, rows = NULL)
    list(plot = p, table = g_table)
    #p
  })
  
  # プロット表示
  output$forest_plot <- renderPlot({
    generate_plot_and_table()
    #    grid.arrange(result$plot, result$table, ncol = 2, widths = c(1, 2))
  })
  
  # プロットダウンロード
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("forest_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      result <- generate_plot_and_table()
      #g <- arrangeGrob(result$table, result$plot, ncol = 2, widths = c(1, 2))
      #ggsave(file, g, width = 14, height = 8)
      ggsave(file, generate_plot_and_table(), width = 10, height = 8)
    }
  )
}

# アプリケーション実行
shinyApp(ui = ui, server = server)