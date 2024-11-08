library(tidyverse)
library(tidyquant)
library(IBrokers)  

tws <- twsConnect(port = 7496) #to connect with TWS         

source(paste0(getwd(), "/FuturesInfo.R"))
FutToBePrepared<-GetFutInfo(tws, FUT=c("NQ"),interval=c("1H","30F","5F","1F"), RealData=TRUE,LoadData=FALSE)
Sys.sleep(3)
twsDisconnect(tws)#to disconnect
