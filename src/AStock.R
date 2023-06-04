library(Tushare)
library(tidyverse)
library(tidyquant)
library(plotly)



A_STOK<-c("LvMengKeJi")
source("src/StockInfo.R")
StkToBePrepared<-GetStockInfo(STK = c("300369.SZ"), interval = c("daily"), GlobalMarket = "China", A_STOK)
source("src/PrepStock.R")

source("src/MyStrategy.R")
ReadCombData(OutputCombtxt,nam)     #This src load the combined data



#Plot
source("src/StockPlotFunction.R") #everytime we run a function from a different src, we must run this command
nam
StockChart(ZhongBinHongJian_daily, Title = "ZhongBinHongJian_daily")
MultiChart(list(AiErYanKe_daily=AiErYanKe_daily,AiErYanKe_weekly=AiErYanKe_weekly))
MultiChart(list(XianDaoZhiNeng_daily=XianDaoZhiNeng_daily,XianDaoZhiNeng_weekly=XianDaoZhiNeng_weekly))
MultiChart(list(LongJiLvNeng_daily=LongJiLvNeng_daily,LongJiLvNeng_weekly=LongJiLvNeng_weekly))


source("src/MACDPower.R") #everytime we run a function from a different src, we must run this command
MACDPower(DataToBeTested=ZhongQiGuFeng_daily, BarOverride=FALSE)

Periods <- data.frame(In1=as.POSIXct("2020-08-03",tz="UTC"),
                         In2=as.POSIXct("2020-10-20",tz="UTC"),
                         Out1=as.POSIXct("2022-01-17",tz="UTC"),
                         Out2=as.POSIXct("2022-03-16",tz="UTC"))

MACDPower(DataToBeTested=ZhongQiGuFeng_daily, Period=Periods, BarOverride=TRUE) #put the data and your own Periodxxx






