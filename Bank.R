filename.1 <- "201701010000_01.csv" # CSVダウンロード
filename.2 <- "s1.txt"          # 画面に表示されたテーブルをコピペ
filename.3 <- "s2.txt"          # 画面に表示されたテーブルをコピペ

# 日付文字列の書式
format.1 <- "%Y%m%d"
format.2 <- "%Y年%m月%d日"
format.3 <- "%Y/%m/%d"

# 列の編集
order.1 <- c(1,3,4,5,6,7)
order.2 <- c(1,2,3,4,6,5)
order.3 <- c(1,4,3,2,6,5)

# 列名の編集
colsnames <- c("取引日", "受入金額", "払出金額", "詳細１", "詳細２", "現在高")

# 集計開始日
since.1 <- "20160401"
since.2 <- "2016年04月01日"
since.3 <- "2016/04/01"

# 列の編集
colsort <- function(data, order){
  data <- data[ , order]
  
  data[ , 2] <- ifelse(is.na(data[ , 2]), 0,  data[ , 2])
  data[ , 3] <- ifelse(is.na(data[ , 3]), 0,  data[ , 3])
  data[ , 5] <- ifelse(is.na(data[ , 5]), "", data[ , 5])
  data[ , 6] <- ifelse(is.na(data[ , 6]), 0,  data[ , 6])
  
  data[ , 2] <- gsub(",", "",  data[ , 2])
  data[ , 3] <- gsub(",", "",  data[ , 3])
  data[ , 6] <- gsub(",", "",  data[ , 6])
  data[ , 2] <- gsub("円", "", data[ , 2])
  data[ , 3] <- gsub("円", "", data[ , 3])
  data[ , 6] <- gsub("円", "", data[ , 6])
  data[ , 6] <- gsub(" ", "",  data[ , 6])
  
  data[ , 2] <- as.numeric(data[ , 2])
  data[ , 3] <- as.numeric(data[ , 3])
  data[ , 6] <- as.numeric(data[ , 6])
  
  colnames(data) <- colsnames
  return( data )
}

# 行の編集
rowfilter <- function(data, format, since){
  Sys.setlocale("LC_TIME","C")
  data[ , 1] <- as.Date(
    as.character(data[ , 1]),
    format=format
  )
  cond <- data$取引日 >= as.Date(since, format=format)
  data <- data[cond, ]
  return( data )
}

read.1 <- function(){
  data.1 <- read.csv(
    filename.1,
    header=T,
    na.strings="",
    skip=6,
    stringsAsFactors=F
  )
  data.1 <- colsort(data.1, order.1)
  data.1 <- rowfilter(data.1, format.1, since.1)
  return( data.1 )
}

read.2 <- function(){
  data.2 <- read.delim(
    filename.2,
    header=T,
    na.strings="",
    stringsAsFactors=F
  )
  data.2 <- colsort(data.2, order.2)
  data.2 <- rowfilter(data.2, format.2, since.2)
  return( data.2 )
}

read.3 <- function(){
  data.3 <- read.delim(
    filename.3,
    header=T,
    na.strings="",
    stringsAsFactors=F
  )
  data.3 <- colsort(data.3, order.3)
  data.3 <- rowfilter(data.3, format.3, since.3)
  return( data.3 )
}

# 時系列処理のためのxtsパッケージの準備
# install.packages("xts")
library(xts)

# xtsオブジェクト
data.1 <- read.1()
data.2 <- read.2()
data.3 <- read.3()
xts.1 <- xts(data.1[ , 6], order.by=as.POSIXct(data.1$取引日))
xts.2 <- xts(data.2[ , 6], order.by=as.POSIXct(data.2$取引日))
xts.3 <- xts(data.3[ , 6], order.by=as.POSIXct(data.3$取引日))

# 欠損値の修正
xtss <- na.locf(merge(xts.1, xts.2, xts.3))
xtss <- na.locf(xtss, fromLast=TRUE)

# 積み上げ折れ線グラフ作成
xtss$xts.2 <- xtss$xts.2 + xtss$xts.3
xtss$xts.1 <- xtss$xts.1 + xtss$xts.2
cols <- c("red", "blue", "green")
xl <- c(
  index(xtss)[1],
  index(xtss)[nrow(xtss)]
  )
yl <- c(
  0,
  1000000
  )
par(new=F)
for(i in 1:ncol(xtss)){
  plot(
    index(xtss),
    xtss[ , i],
    col=cols[i],
    type="l",
    xlim=xl,
    ylim=yl
    )
  par(new=T)
}
