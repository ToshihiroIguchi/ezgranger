library(shiny)

#タイトルの定義
ui.title <- "Easy Granger causality"

#単位根検定の方法
diff.test <- c("adf", "pp")
names(diff.test) <- c("Augmented Dickey–Fuller Test", "Phillips–Perron Unit Root Test")


shinyUI(fluidPage(
  
  titlePanel(title = "", windowTitle = ui.title),
  
  sidebarLayout(
    sidebarPanel(
      
      #ファイル読み込み
      fileInput("file", "Data file (.csv)", accept = c(".csv")),
      
      htmlOutput("tr1.name"),
      htmlOutput("tr2.name"),
      
      htmlOutput("p.th"),
      
      htmlOutput("tr1.diff"),
      htmlOutput("tr2.diff"),
      
      htmlOutput("tr.range")
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  #ファイルの内容をそのまま表示
                  tabPanel("Original",
                           plotOutput("raw.trend.plot")
                           ),
                  
                  tabPanel("Granger causality",
                           plotOutput("diff.trend.plot"),
                           verbatimTextOutput("sum.gr")
                           ),
      
                  tabPanel("Setting",
                           
                           #単位根検定の方法
                           selectInput("ur.test", "Method of unit root test",
                                       choices = diff.test),
                           
                           
                           #ラグの最大次数
                           numericInput("maxlag", "Highest lag order", 
                                                          value = 10, min = 1, max = NA, step = 1),
                           
                                   
                           
                           #決定論的な説明変数の型
                           selectInput("var.type", "Type of deterministic regressors to include",
                                         choices = c("const", "trend", "both", "none"),
                                         selected = "const"),
                           selectInput("ord.sel", "Order selection criteria",
                                         choices = c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)"),
                                         selected = "AIC(n)"),
                           
                           #グラフの文字の大きさ
                           numericInput("font.size", "Font size", value = 12, min = 1, max = NA, step = 0.5)
                           
                           
                           
                           )
                  
                  )
      
      
      
      
      
       
      
      
    )
  )
))
