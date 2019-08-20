#ライブラリ読み込み
library(tseries) #installなしでOK
library(vars)
library(ggplot2)
library(grid) #installなしでOK


#強制ベクトル変換
as.vec <- function(x){
  ret <- as.vector(as.matrix(x))
  return(ret)
}

#解析する価値があるか
is.worth <- function(vec){
  
  #数値でなければFALSE
  if(!is.numeric(vec)){return(FALSE)}
  
  #差分の標準偏差が0ならFALSEを返す
  if(sd(diff(vec)) == 0){return(FALSE)}
  
  #どれにも該当しなければTRUE
  return(TRUE)
  
}

#自動差分
auto.diff <- function(vec, differences = NULL, test = "adf", p.th = 0.05){
  
  #エラーチェック
  if(is.null(vec)){return(NULL)}

  #時系列に変換
  vec <- as.ts(vec)
  
  #differencesが数値でない場合の処理
  #shiny上でcharacterとして選択する場合がある
  if(!is.numeric(differences)){
    if(differences == "Auto"){
      differences <- NULL
    }else{
      differences <- as.numeric(differences)
    }
    
  }
  
  #differencesが指定された場合の処理
  if(!is.null(differences)){
    
    #differences = 0だとエラーが出るので場合分け
    if(differences == 0){
      ret <- vec
      return(ret)
    }else{
      ret <- diff(x = vec, differences = differences)
      return(ret)
    }
    
    
  }
  
  #iの大きさを変えていく
  for(i in 0:(length(vec) - 1)){
    if(i == 0){
      #差分iが0なら元のデータを入れる。
      #diff differences = 0だとエラーになるため。
      d.vec <- vec
    }else{
      d.vec <- diff(x = vec, differences = i)
    }
    
    if(sd(d.vec) == 0){return(d.vec)}
    
    #ADF検定
    if(test == "adf"){
      suppressWarnings(
        p.val <-   adf.test(d.vec)$p.value
      )
    }
    
    #PP検定
    if(test == "pp"){
      suppressWarnings(
        p.val <- pp.test(d.vec)$p.value
      )
    }
    
    #単位根(p.valが閾値(例えば0.05)より大きい）を棄却したら、
    #breakしてループから抜ける
    if(p.val <= p.th){
      break()
    }

  }
  
  #iの値に応じて差分を返す。
  if(i == 0){
    ret <- vec
  }else{
    ret <- diff(vec, differences = i)
  }
  

  #戻り値
  return(ret)
  
}

#データフレームから価値のある項目名のみ抜き出す
df.num.names <- function(df){
  
  ret <- colnames(df)[sapply(df, is.worth)]
  ret <- as.vector(as.matrix(ret))
  
  return(ret)

}

#時系列をデータフレームに
trend.df <- function(trend1, trend2, name.tr1, name.tr2){
  
  #エラーチェック
  if(is.null(trend1) || is.null(trend2) || is.null(name.tr1) || is.null(name.tr2)){return(NULL)}
  
  #長さを確認
  n.tr1 <- length(trend1)
  n.tr2 <- length(trend2)
  
  if(n.tr1 == n.tr2){
    df <- data.frame(tr1 = trend1, tr2 = trend2)
  }
  
  if(n.tr1 < n.tr2){
    df <- data.frame(tr1 = trend1, tr2 = trend2[-c(1 : (n.tr2 - n.tr1))])
  }
  
  if(n.tr2 < n.tr1){
    df <- data.frame(tr1 = trend1[-c(1 : (n.tr1 - n.tr2))], tr2 = trend2)
  }
  
  #データフレームの名前
  colnames(df) <- c(name.tr1, name.tr2)
  
  
  return(df)
}

#データフレームから範囲と名前を指定
df.range.name <- function(df, range, name){
  
  #エラーチェック
  if(is.null(df) || is.null(range) || is.null(name)){return(NULL)}
  
  ret <- as.vector(as.matrix(df[c(range[1] : range[2]), name]))
  
  return(ret)
  
  
}

#Grangerの因果性検定
ezgranger <- function(trend1, trend2, name.tr1, name.tr2, 
                      type = "const", lag.max  = 5, 
                      rect = TRUE,
                      p.th = 0.05, sel = "AIC(n)"){
  
  #エラーチェック
  if(is.null(trend1) || is.null(trend2) || is.null(name.tr1) || is.null(name.tr2)){return(NULL)}
  
  #エラーチェック2
  if(name.tr1 == name.tr2){
    stop("Granger causality between the same item names cannot be calculated.")
  }
  
  #データフレーム作成
  df <- trend.df(trend1, trend2, name.tr1, name.tr2)
  
  #次数の選択
  var.sel <- VARselect(df, lag.max = lag.max, type = type)$selection
  
  #使う次数を使ってlagを計算
  lag <- var.sel[sel]
  
  #多変量自己回帰分析
  var.res <- VAR(df, p = lag, type = type)
  
  #因果性の検定
  var.cause1 <- causality(var.res, cause = name.tr1)
  var.cause2 <- causality(var.res, cause = name.tr2)
  
  #p値
  p.tr1 <- var.cause1$Granger$p.value
  p.tr2 <- var.cause2$Granger$p.value
  
  #メッセージ
  tr.txt <- paste0("Granger causality H0: ",
                   name.tr1, " do not Granger-cause ", name.tr2, "\np.value = ", 
                   format(p.tr1, digits = 4),
                   
                   "\n\n",
                   "Granger causality H0: ",
                   name.tr2, " do not Granger-cause ", name.tr1, "\np.value = ", 
                   format(p.tr2, digits = 4),
                   "\n\n")
  
  #因果関係の矢印 #cause1 <---> cause2
  if(p.tr1 <= p.th){tr1.ar <- ">"}else{tr1.ar <- ""}
  if(p.tr2 <= p.th){tr2.ar <- "<"}else{tr2.ar <- ""}
  
  #矢印作成
  tr.arrow <- paste0(name.tr1, " ", tr2.ar, "---", tr1.ar, " ", name.tr2)
  
  #文字を結合
  ret.txt <- paste0(tr.txt, tr.arrow)
  
  #戻り値をリストで定義
  ret <- list(df = df, p.tr1 = p.tr1, p.tr2 = p.tr2, txt = ret.txt)
  
  #クラスを定義
  class(ret) <- "granger"
  
  #戻り値
  return(ret)
}

#グレンジャー因果の結果表示
summary.granger <- function(result){
  
  if(is.null(result)){return(NULL)}
  
  cat(result$txt)
}

#時系列をプロット
plot.trend <- function(trend1, trend2, name.tr1, name.tr2, 
                       rect = TRUE,
                       range = NULL, x.lab = "No" ,text.size = 12){
  
  #エラーチェック
  if(is.null(trend1) || is.null(trend2) || is.null(name.tr1) || is.null(name.tr2)){return(NULL)}
  
  #データフレーム作成
  df <- trend.df(trend1, trend2, name.tr1, name.tr2)
  
  #Noをつけくわえる
  df <- data.frame(No = c(1:nrow(df)), df)
  
  #rangeを指定
  if(is.null(range)){
    range <- c(1, nrow(df))
  }
  
  
  #ggplot
  gg1 <- ggplot(data = df, aes_(x = as.name("No"), y = as.name(name.tr1))) + geom_line() + 
    xlab(x.lab) + ylab(name.tr1) +
    theme(axis.text = element_text(size = text.size)) +
    if(rect){annotate("rect", xmin = range[1], xmax = range[2], 
             ymin = min(df[, name.tr1]), ymax = max(df[, name.tr1]),
             alpha = 0.1, fill = "green")
    }else{
      scale_x_continuous(breaks=NULL)
      }
  
  gg2 <- ggplot(data = df, aes_(x = as.name("No"), y = as.name(name.tr2))) + geom_line() + 
    xlab(x.lab) + ylab(name.tr2) +
    theme(axis.text = element_text(size = text.size)) +
    if(rect){annotate("rect", xmin = range[1], xmax = range[2], 
             ymin = min(df[, name.tr2]), ymax = max(df[, name.tr2]),
             alpha = 0.1, fill = "green")
    }else{
      scale_x_continuous(breaks=NULL)
      }
  
  #グリッド分割
  #https://winlabo.com/post-812
  grid.newpage()#空の画面を作る
  pushViewport(viewport(layout = grid.layout(2, 1)))#画面を区切る
  
  #画像表示
  print(gg1, vp = viewport(layout.pos.row=1, layout.pos.col=1))
  print(gg2, vp = viewport(layout.pos.row=2, layout.pos.col=1))

}




