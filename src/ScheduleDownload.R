library(tidyverse)
library(tidyquant)
library(IBrokers)    

tws <- twsConnect(port = 7496) #to connect with TWS         

source("/Users/tengli/R/TradingAlg/src/FuturesInfo.R")
FutToBePrepared<-GetFutInfo(FUT=c("NQ"),interval=c("5F"))
source("/Users/tengli/R/TradingAlg/src/PrepFutures.R")  #This src downloads the data and prepare for combination

twsDisconnect(tws)#to disconnect
