#######################################################################################################
#_____________________________________________INSTRUCTION______________________________________________
#######################################################################################################

#Step 0-----------------------------
#run the following every
#time you open Rstudio
#or if you change any
#of the source codes 
#below.

library(tidyverse)
library(tidyquant)
library(plotly)

source("Script/Candy/GetData_Candy.R")
source("Script/ChanLunFunction.R")
source("Script/StockPlotFunction.R")
source("Script/MACDPower.R")
source("Script/SignalPlot.R")
source("Script/ChartReplay.R")

av_api_key("<77P2LJBQ51UX69O7>")

#Step 1:-----------------------------
#User needs to specify the following:
#SecurityType: STK for stock, FOREX for forex
#Symb: "AMD", or "CAD/USD"
#intv: 1min, 5min, 30min, daily, weekly, monthly for stocks 
#NumOfMonths: the number of months of the intraday data you want. If daily/weekly/monthly, always put 1
#GlobalMarket: US or China
#OutputSize:  full returns the full-length time series of 20+ years of historical data of the stock, compact has the latest 100 data points


SecurityType = c("STK")               
Symb <-data.frame(Symb=c("QQQ","USO"))      
intv <-c("daily","weekly")             
NumOfMonths <-c(1)                    
GlobalMarket <- c("US")                                             
OutputSize <- "full"                  

source("Script/Candy/PrepStock_Candy.R")


#Step 2:-----------------------------
#Please combine the data using the CandleStickApp


#Step 3:-----------------------------
#This script load the combined data

ReadCombData(OutputCombtxt,nam)     


#Step 4------------------------------------------
#StockChart plots a single stock.
#MultiChart plots multiple charts.

StockChart(QQQ_daily, Title = "QQQ_daily")

MultiChart(list(QQQ_weekly=QQQ_weekly,QQQ_daily=QQQ_daily))
MultiChart(list(USO_weekly=USO_weekly,USO_daily=USO_daily))

SignalPlot(list(QQQ_daily=QQQ_daily))
MultiSignalChart(list(QQQ_daily=QQQ_daily,QQQ_weekly=QQQ_weekly))


#Step 5------------------------------------------
#MACDPower generates signals for a single stock of 
#a time scale.
#CoDivergence generates signals for a single stock
#with multiple time scales.

MACDPower(DataToBeTested=QQQ_daily)
CoDivergence(DataToBeTested=list(QQQ_daily=QQQ_daily,QQQ_weekly=QQQ_weekly), BarOverride=c(FALSE,FALSE))

#OR specify your own bar start and end dates: "2019-10-04 12:00:00"
PeriodQQQ_daily <- data.frame(In1=as.POSIXct("2021-11-19"),
                         In2=as.POSIXct("2022-01-24"),
                         Out1=as.POSIXct("2022-08-15"),
                         Out2=as.POSIXct("2022-10-11"))

PeriodQQQ_weekly <- data.frame(In1=as.POSIXct("2021-10-28 08:50:00",tz="UTC"),
                         In2=as.POSIXct("2021-10-28 11:40:00",tz="UTC"),
                         Out1=as.POSIXct("2021-10-28 12:55:00",tz="UTC"),
                         Out2=as.POSIXct("2021-10-28 14:35:00",tz="UTC"))

MACDPower(DataToBeTested=QQQ_weekly, Period=PeriodQQQ_daily, BarOverride=TRUE) #put the data and your own Periodxxx
CoDivergence(DataToBeTested=list(QQQ_daily=QQQ_daily,QQQ_weekly=QQQ_weekly),Period=list(PeriodQQQ_daily, NULL), BarOverride=c(TRUE,FALSE)) 


#Step 6------------------------------------------
#This section is for chart replay. 
##################################
#-----------WARNING--------------#
#User must periodically clear the 
#Viewer pane on the bottom right-
#hand side.
##################################

#If you want to start from the beginning, with PausePeriod being 1 second:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", PausePeriod=3, UerInput = "N")

#if you know which candlestick you want to start from:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", StartCandle=97, UerInput = "N")

#or use the date to specify:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", StartDate="2000-04-14", UerInput = "N")








#######################################################################################################
#________________________________________Additional Features___________________________________________
#######################################################################################################
#Portfolio weight returns
source("/Users/tengli/R/Script/Portfolio Weight.R")
MaxPortfolio(DataName=c("MARA_daily", "NVDA_daily", "AAPL_daily"), Bookcost = 100000)



#######################################################################################################
#######################################################################################################
#Calculate the maximum number of positions you can add

source("/Users/tengli/R/Script/PL_Change.R") #everytime we run a function from a different script, we must run this command
MaxPosition(Profit=158.1, LossPercent=0.75, Currentprice=14784, Stoploss=14822, Leverage=2)



#######################################################################################################
#######################################################################################################
#Options
source("/Users/tengli/R/Script/OptionCalculator.R")
BlackScholes(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type = "C")
Greeks(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type = "C")
DGTOptionModel(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type="C",PredictedP=205,day=1)


