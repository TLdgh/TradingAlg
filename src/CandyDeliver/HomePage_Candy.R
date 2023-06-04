#Step 0: run the following commands everytime you open R. You only need to run once if you don't close R.
library(tidyverse)
library(tidyquant)
library(plotly)
library(timetk)
library(ggrepel)
library(ggpubr)

source("C:\\R\\Script\\GetData_Candy.R")
source("C:\\R\\Script\\StockPlotFunction_Candy.R") #everytime we run a function from a different script, we must run this command
source("C:\\R\\Script\\SignalPlot_Candy.R") #everytime we run a function from a different script, we must run this command
source("C:\\R\\Script\\MACDPower_Candy.R") #everytime we run a function from a different script, we must run this command


#STEP 1: enter the Symbol and the time interval you want
av_api_key("<77P2LJBQ51UX69O7>")
SecurityType = c("STK")               #SecurityType: STK for stock, FOREX for forex
Symb <-data.frame(Symb=c("QQQ"))      #Symb: "AMD", or "CAD/USD"
intv <-c("30min")                     #intv: 1min, 5min, 30min, daily, weekly, monthly for stocks 
NumOfMonths <-c(5)                    #NumOfMonths: the number of months of the intraday data you want. If daily/weekly/monthly, always put 1
GlobalMarket <- c("US")                                             
OutputSize <- "full"                  #OutputSize:  full returns the full-length time series of 20+ years of historical data of the stock, compact has the latest 100 data points

source("C:\\R\\Script\\PrepStock_Candy.R")

#STEP 2:
#Please combine the data using the CandleStickApp, then proceed with the next step

#STEP 3:
ReadCombData(OutputCombtxt,nam)     #This script load the combined data

################Price plotly
nam
StockChart(QQQ_daily, Title = "QQQ_daily")


MultiChart(list(BITF5min=BITF5min,BITF30min=BITF30min)) #if you want to add more, always put the name in the same way XXX=XXX




#######################################################################################################
#######################################################################################################
#Compare the MACD bar area

SignalPlot(list(QQQ_daily=QQQ_daily))
MultiSignalChart(list(QQQ_daily=QQQ_daily,NQ5F=NQ5F,NQ30F=NQ30F))

MACDPower(DataToBeTested=QQQ_daily, BarOverride=FALSE) #BarOverride=FALSE compares the signals for the latest ZhongShu
CoDivergence(DataToBeTested=list(QQQ_daily=QQQ_daily,QQQ_daily=QQQ_daily), BarOverride=c(FALSE,FALSE)) #enter the data you want like "xxx = xxx", this will create the data and the name in the list

#OR specify your own bar start and end dates: "2019-10-04 12:00:00"
PeriodNQ1F <- data.frame(In1=as.POSIXct("2021-11-01 10:00:00",tz="UTC"),
                         In2=as.POSIXct("2021-11-05 10:00:00",tz="UTC"),
                         Out1=as.POSIXct("2021-11-15 12:30:00",tz="UTC"),
                         Out2=as.POSIXct("2021-11-22 10:30:00",tz="UTC"))

PeriodNQ5F <- data.frame(In1=as.POSIXct("2021-10-28 08:50:00",tz="UTC"),
                         In2=as.POSIXct("2021-10-28 11:40:00",tz="UTC"),
                         Out1=as.POSIXct("2021-10-28 12:55:00",tz="UTC"),
                         Out2=as.POSIXct("2021-10-28 14:35:00",tz="UTC"))

MACDPower(DataToBeTested=NQ1F, Period=PeriodNQ1F, BarOverride=TRUE) #put the data and your own Periodxxx
CoDivergence(DataToBeTested=list(CAD5F=CAD5F,CAD30F=CAD30F),Period=list(PeriodCAD5F, NULL), BarOverride=c(TRUE,FALSE)) 



#######################################################################################################
#######################################################################################################
#Portfolio weight returns
source("C:\\R\\Script\\PortfolioWeight_Candy.R")
MaxPortfolio(DataName=c("QQQ_daily", "NVDA_daily", "AAPL_daily"), Bookcost = 100000)




#######################################################################################################
#######################################################################################################
#Calculate the maximum number of positions you can add

source("C:\\R\\Script\\PL_Change_Candy.R") #everytime we run a function from a different script, we must run this command
MaxPosition(Profit=158.1, LossPercent=0.75, Currentprice=14784, Stoploss=14822, Leverage=2)
