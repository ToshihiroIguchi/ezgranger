#ライブラリ読み込み
library(shiny)

#ソース読み込み
source("ezgranger.R")

#差分の指定
diff.vec <- c("Auto", "0", "1", "2", "3", "4", "5")




shinyServer(function(input, output) {
  
  #ファイルを選択した場合
  observeEvent(input$file, {
    
    #ファイル名表示
    print(input$file$name)
    
    #ファイル読み込み
    raw.data <- reactive({
      read.csv(input$file$datapath)
      })
    
    #解析可能な項目のなまえ
    num.names <- reactive({
      df.num.names(raw.data())
    })
    
    #時系列1選択
    output$tr1.name = renderUI({
      selectInput("tr1.name", "Trend1 name",
                  choices = num.names(),
                  selected = num.names()[1])
      })
    
    #時系列2選択
    output$tr2.name = renderUI({
      selectInput("tr2.name", "Trend2 name",
                  choices = num.names(), 
                  selected = num.names()[2])
      })
    
    #p値の閾値
    output$p.th = renderUI({
      selectInput("p.th", "p-value", 
                  choices = c("0.01", "0.05", "0.1"), selected = "0.05")
    })
    
    #時系列1の差分
    output$tr1.diff = renderUI({
      selectInput("tr1.diff", "Trend1 iterated differences",
                  choices = diff.vec)
      })
    
    #時系列2の差分
    output$tr2.diff = renderUI({
      selectInput("tr2.diff", "Trend2 iterated differences",
                  choices = diff.vec)
      })
    
    #時系列の範囲
    output$tr.range = renderUI({
      sliderInput("tr.range", "Trend range",
                                            min = 1,
                                            max = nrow(raw.data()),
                                            value = c(1, nrow(raw.data())),
                                            step = 1)
      })
    
    #時系列1の区間と差分をとったベクトル
    tr1.diff.vec <- reactive({
      auto.diff(
        df.range.name(raw.data(), input$tr.range, input$tr1.name),
        differences = input$tr1.diff,
        test = input$ur.test,
        p.th = as.numeric(input$p.th))
    })
    
    #時系列2の区間と差分をとったベクトル
    tr2.diff.vec <- reactive({
      auto.diff(
        df.range.name(raw.data(), input$tr.range, input$tr2.name),
        differences = input$tr2.diff,
        test = input$ur.test,
        p.th = as.numeric(input$p.th))
    })

    #元データの時系列表示
    output$raw.trend.plot = renderPlot({ 
               plot.trend(
                 trend1 = raw.data()[, input$tr1.name], 
                 trend2 = raw.data()[, input$tr2.name], 
                 name.tr1 = input$tr1.name, 
                 name.tr2 = input$tr2.name, 
                 rect = TRUE,
                 range = input$tr.range, 
                 text.size = input$font.size,
                 x.lab = "No"
               )})
    
    #差分データの時系列表示
    output$diff.trend.plot = renderPlot({ 
      plot.trend(
        trend1 = tr1.diff.vec(),  
        trend2 = tr2.diff.vec(), 
        name.tr1 = input$tr1.name, 
        name.tr2 = input$tr2.name , 
        rect = FALSE ,
        text.size = input$font.size,
        x.lab = NULL
      )})

    #Grangerの因果性検定を自動差分を取った時系列で行う
    granger <- reactive({
      ezgranger(trend1 = tr1.diff.vec(), 
                trend2 = tr2.diff.vec(),
                name.tr1 = input$tr1.name, 
                name.tr2 = input$tr2.name, 
                type = input$var.type, 
                lag.max  = input$maxlag,
                p.th = as.numeric(input$p.th), 
                sel = "AIC(n)")
    })
    
    #単位根検定の結果表示
    output$sum.gr = renderPrint({
      summary(granger())
    })
    
    
    })
   

  
})
