library(tidyverse)
library(tidyquant)
library(IBrokers)    

tws <- twsConnect(port = 7496) #to connect with TWS         

source("Script/FuturesInfo.R")
FutToBePrepared<-GetFutInfo(FUT=c("NQ"),interval=c("1F"))
source("Script/PrepFutures.R")  #This script downloads the data and prepare for combination
