#######################################################################################################
#_____________________________________________INSTRUCTION______________________________________________
#STK is the symbol: e.g. "AAPL"
#interval: 5F, 30F, daily etc.
#follow the steps to create a chart
#######################################################################################################


#Step 0------------------
#run the following every
#time you open Rstudio
#or if you change any
#of the source codes 
#below.

library(tidyverse)
library(tidyquant)
library(plotly)
library(IBrokers)

source("src/StockInfo.R")
source("src/MyStrategy.R")
source("src/ChanLunFunction.R")
source("src/StockPlotFunction.R")
source("src/MACDPower.R")
source("src/SignalPlot.R")

#User must specify the IB port number
tws <- twsConnect(port = ) #to connect with TWS
isConnected(tws)#check if connected or not
twsConnectionTime(tws)# check what time did you connect
twsDisconnect(tws)#to disconnect


#Step 1------------------------------------------
#IMPORTANT! If the data does not exist, the user 
#must initialize it by using InitializeStock.R.
#Details please refer to the src.

#Once the user has downloaded the initial data,
#the user can use the following to update:

StkToBePrepared<-GetStockInfo(STK = c("TSLA","GOOGL","AAPL","MSFT","AMD"), interval = c("30F","daily"))
source("src/PrepStock.R")


#Step 2------------------------------------------
#Please combine the data using the CandleStickApp


#Step 3------------------------------------------
#This src load the combined data

ReadCombData(OutputCombtxt,nam) 


#Step 4------------------------------------------
#StockChart plots a single stock.
#MultiChart plots multiple charts.

StockChart(AMD30F, Title = "AMD30F")
StockChart(AMD_daily, Title = "AMD_daily")

StockChart(GOOGL30F, Title = "GOOGL30F")
StockChart(GOOGL_daily, Title = "GOOGL_daily")

StockChart(TSLA30F, Title = "TSLA30F")
StockChart(TSLA_daily, Title = "TSLA_daily")

StockChart(AAPL30F, Title = "AAPL30F")
StockChart(AAPL_daily, Title = "AAPL_daily")

StockChart(MSFT30F, Title = "MSFT30F")
StockChart(MSFT_daily, Title = "MSFT_daily")

MultiChart(list(GOOGL_daily=GOOGL_daily,GOOGL30F=GOOGL30F))
MultiChart(list(AMD_daily=AMD_daily,AMD30F=subset(AMD30F,Date>="2021-01-01 09:30:00")))
MultiChart(list(TSLA_daily=TSLA_daily,TSLA30F=TSLA30F))
MultiChart(list(AAPL_daily=AAPL_daily,AAPL30F=AAPL30F))
MultiChart(list(MSFT_daily=MSFT_daily,MSFT30F=MSFT30F))

SignalPlot(list(NQ1F=NQ1F))
MultiSignalChart(list(NQ1F=NQ1F,NQ5F=NQ5F,NQ30F=NQ30F))


#Step 5------------------------------------------
#MACDPower generates signals for a single stock of 
#a time scale.
#CoDivergence generates signals for a single stock
#with multiple time scales.

MACDPower(DataToBeTested=NQ1F)
CoDivergence(DataToBeTested=list(NQ1F=NQ1F,NQ5F=NQ5F), BarOverride=c(FALSE,FALSE)) 

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


#Step 6------------------------------------------
#This section is for chart replay. 
##################################
#-----------WARNING--------------#
#User must periodically clear the 
#Viewer pane on the bottom right-
#hand side.
##################################

#If you want to start from the beginning, with PausePeriod being 1 second:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", PausePeriod=1, UerInput = "N")

#if you know which candlestick you want to start from:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", StartCandle=90, UerInput = "N")

#or use the date to specify:
ChartReplay(Pricedata=QQQ_daily,Title="QQQ_daily", StartDate="2000-04-14", UerInput = "N")






#######################################################################################################
#________________________________________Additional Features___________________________________________
#######################################################################################################
#Portfolio weight returns
source("src/Portfolio Weight.R")
MaxPortfolio(DataName=c("MARA_daily", "NVDA_daily", "AAPL_daily"), Bookcost = 100000)



#######################################################################################################
#######################################################################################################
#Calculate the maximum number of positions you can add

source("src/PL_Change.R") #everytime we run a function from a different src, we must run this command
MaxPosition(Profit=158.1, LossPercent=0.75, Currentprice=14784, Stoploss=14822, Leverage=2)



#######################################################################################################
#######################################################################################################
#Options
source("src/OptionCalculator.R")
BlackScholes(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type = "C")
Greeks(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type = "C")
DGTOptionModel(S=200,K=200,r=0.05,delt=0,TimeToExpiry=30/365,sig=0.4,type="C",PredictedP=205,day=1)



#######################################################################################################
#######################################################################################################
#Automatic detecting trendreversal:
#Step1: run the calc on top of this page to get the latest planet information of your data as an initial Pivotalplanet setup.
DataToAlert<-list(subset(AMD30F, Date>="2022-12-29 01:30:00"))
Pivotalplanet<-data.frame()
for (i in 1:length(DataToAlert)) {
  Pivotalplanet<-rbind(Pivotalplanet,tail(subset(as.data.frame(PlanetFunction(StarFunction(DataToAlert[[i]]))), PlanetHigh!=0),1))
}
write.csv(Pivotalplanet,file=paste0(getwd(),"/CandleStickComb/Pivotalplanet.csv"),row.names = FALSE)

#Step2: go to ScheduleDownload.R to update the contract you want to check and run CandleApp
#Step3: if you need to get the updated data, run the following:
OutputCombtxt<-readLines(paste0(getwd(),"/CandleStickComb/OutputLoc.txt"))
nam<-gsub(pattern=".*[/](.+)Comb.CSV.*",replacement = "\\1", x=OutputCombtxt)
source("src/MyStrategy.R")
ReadCombData(OutputCombtxt=OutputCombtxt,nam)     #This src load the combined data
StockChart(AMD30F, Title = "AMD30F")










