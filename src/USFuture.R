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

#library(timetk)
#library(ggrepel)
#library(ggpubr)
#library(meboot)
#library(KSgeneral)

library(tidyverse)
library(tidyquant)
library(plotly)
library(IBrokers)

#User must specify the IB port number
tws <- twsConnect(port = 7496) #to connect with TWS
isConnected(tws)#check if connected or not
twsConnectionTime(tws)# check what time did you connect
twsDisconnect(tws)#to disconnect

source("src/FuturesInfo.R")
source("src/ChanLunFunction.R")
source("src/StockPlotFunction.R")
source("src/MACDPower.R")
source("src/SignalPlot.R")
source("src/Bootstrap.R")


#Step 1------------------------------------------
FutToBePrepared<-GetFutInfo(tws, FUT=c("NQ"),interval=c("30F","1H"), RealData=TRUE)

#Step 2------------------------------------------
#Please combine the data using the CandleStickApp

#Step 3------------------------------------------
#This src load the combined data
#FutToBePrepared$ReadCombData() 


#Step 4------------------------------------------
#StockChart plots a single stock.
#MultiChart plots multiple charts.

StockChart(NQ30F)

StockChart(NQ5F)

StockChart(NQ1F)

StockChart(NQContinuous,VIXfile = "VXN",VOIdata = VOI)

MultiChart(list(NQ5F=NQ5F,NQ1F=NQ1F))

MultiChart(list(NQ30F=NQ30F, NQ5F=NQ5F))

MultiChart(list(NQ5F=NQ5F,NQ30F=NQ30F,NQ4H=NQ4H))

MultiChart(list(NQ30F=subset(NQ30FContinuous,Date>="2024-05-01"),NQ4H=subset(NQ4HContinuous,Date>="2024-01-01"),NQContinuous=NQContinuous,NQWContinuous=NQWContinuous))

SimTrend(NQContinuous,n=2,CombineSim= TRUE)


################ Futures COT and VOI price chart 
#First use ContinuousContract to download the continuous daily data.
#Second use VOI to prepare the VOI data.
VOI<-read.csv("CMEVOI/NQ_DailyVOI.csv")
COT <- read.csv("CFTC_OI/NQ_Open_Interest.csv", header = T)
COT$COT_NonCom <-(COT$NonComm_Positions_Long_All+COT$NonComm_Postions_Spread_All) - (COT$NonComm_Positions_Short_All+COT$NonComm_Postions_Spread_All)
COT$COT_Com <- COT$Comm_Positions_Long_All - COT$Comm_Positions_Short_All
COT$COT_NonRept <- COT$NonRept_Positions_Long_All - COT$NonRept_Positions_Short_All

#DO NOT COMBINE THE CONTINUOUS DATA, WE WANT TO CHECK THE COT AND OPEN INTEREST. IF YOU WANT TO SEE THE COMBINED DATA WITH 中枢, PLEASE USE THE TOP FUTURES SECTION.

COTContinuous<-read.csv("Data/NQ/NQContinuous.csv",header = TRUE) #this is the raw data, without candlestick combination
COTContinuous<-merge(COTContinuous[,1:5], VOI, by.y="Date",all = FALSE)
COT<-COT_na_approx(pricedata = COTContinuous, COT = COT)

source("src/StockPlotFunction.R") #everytime we run a function from a different src, we must run this command
FPVChart(COTContinuous, COT)
FPVCOTChart(COTContinuous, COT)


#Step 5------------------------------------------
#MACDPower generates signals for a single stock of 
#a time scale.
#CoDivergence generates signals for a single stock
#with multiple time scales.

source("src/SignalPlot.R") #everytime we run a function from a different src, we must run this command
FutToBePrepared<-GetFutInfo(FUT=c("NQ"),interval=c("Continuous"))
source("src/PrepFutures.R")
SignalPlot(list(NQContinuous=NQContinuous), AddSignal=TRUE)
MultiSignalChart(list(NQ1F=NQ1F,NQ5F=NQ5F,NQ30F=NQ30F))

MACDPower(DataToBeTested=NQ1F, BarOverride=FALSE)
MACDPower(DataToBeTested=NQ5F)
MACDPower(DataToBeTested=NQ30F)
MACDPower(DataToBeTested=NQ4H)
MACDPower(DataToBeTested=NQContinuous, BarOverride=FALSE)
MACDPower(DataToBeTested=subset(NQ30F, Date<="2023-05-23 02:30:00"))

MACDPower(DataToBeTested=GC30F, BarOverride=FALSE)

MACDPower(DataToBeTested=CAD30F, BarOverride=FALSE)
MACDPower(DataToBeTested=CAD5F, BarOverride=FALSE)
MACDPower(DataToBeTested=CAD1F, BarOverride=FALSE)

#enter the data you want to analyze
CoDivergence(DataToBeTested=list(NQ5F=NQ5F,NQ30F=NQ30F), BarOverride=c(FALSE,FALSE)) #enter the data you want like "xxx = xxx", this will create the data and the name in the list
CoDivergence(DataToBeTested=list(NQ1F=NQ1F,NQ5F=NQ5F,NQ30F=NQ30F), BarOverride=c(FALSE,FALSE,FALSE))

CoDivergence(DataToBeTested=list(CAD1F=CAD1F,CAD5F=CAD5F), BarOverride=c(FALSE,FALSE)) #enter the data you want like "xxx = xxx", this will create the data and the name in the list
CoDivergence(DataToBeTested=list(CAD1F=CAD1F,CAD5F=CAD5F,CAD30F=CAD30F), BarOverride=c(FALSE,FALSE,FALSE)) #enter the data you want like "xxx = xxx", this will create the data and the name in the list


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

#Bootstrap
source("src/BootStrap.R")
BootStrap(DataToBeFit=NQ5F, OriginalData=NQ5FContinuous, nboot = 200)
BootStrap(DataToBeFit=NQ30F, OriginalData=NQ30FContinuous, nboot = 200)
BootStrap(DataToBeFit=NQContinuous, OriginalData=QQQ_daily, nboot = 5000)
BootStrap(DataToBeFit=QQQ_weekly, OriginalData=NQWContinuous, nboot=10000)



#######################################################################################################
#######################################################################################################
#Calculate the maximum number of positions you can add
source("src/PL_Change.R") #everytime we run a function from a different src, we must run this command
MaxPosition(Profit=158.1, LossPercent=0.75, Currentprice=14784, Stoploss=14822, Leverage=2)

#Calculate trading PnL:
data<-read.csv("sample.csv")
PnL(data)



#######################################################################################################
#######################################################################################################
#Automatic detecting trendreversal:
#Step1: go to ScheduleDownload.R to update the contract you want to check and run CandleApp
#Step2: if you need to get the updated data, run the following:
InputLoc<-readLines(paste0(getwd(),"/CandleStickComb/InputLoc.txt"))
nam<-gsub(pattern=".*/|\\.csv.*",replacement = "", x=InputLoc)
for (i in 1:length(InputLoc)){
  FutToBePrepared$DownloadData(nam=nam[i], fileloc = InputLoc[i], LoadData = TRUE)     #This src load the combined data
}
MultiChart(list(NQ5F=NQ5F,NQ30F=NQ30F,NQ4H=NQ4H))
