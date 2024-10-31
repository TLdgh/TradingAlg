library(tidyverse)
library(tidyquant)
library(IBrokers)    

tws <- twsConnect(port = 7496) #to connect with TWS         

source("/Users/tengli/R/TradingAlg/src/FuturesInfo.R")
FutToBePrepared<-GetFutInfo(tws, FUT=c("NQ"),interval=c("5F"), RealData=TRUE,LoadData=FALSE)
Sys.sleep(3)
twsDisconnect(tws)#to disconnect
