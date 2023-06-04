library(tidyverse)
library(tidyquant)
library(IBrokers)    

tws <- twsConnect(port = 7496) #to connect with TWS         

source("src/FuturesInfo.R")
FutToBePrepared<-GetFutInfo(FUT=c("NQ"),interval=c("1F"))
source("src/PrepFutures.R")  #This src downloads the data and prepare for combination
