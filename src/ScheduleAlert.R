library(beepr)
library(quantmod)
library(tidyverse)

source("/Users/tengli/R/TradingAlg/src/ChanLunFunction.R")
source("/Users/tengli/R/TradingAlg/src/StockPlotFunction.R") 
source("/Users/tengli/R/TradingAlg/src/MACDPower.R")

datalocation<-commandArgs(trailingOnly = TRUE)
write.table(datalocation,file="/Users/tengli/R/TradingAlg/CandleStickComb/OutputLoc.txt",sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE) # To be deleted


DataToCheck<-list()
for (i in 1:length(datalocation)){
  DataToCheck[[i]]<-read.csv(file=datalocation[i],header = TRUE)
  DataToCheck[[i]]<-DataToCheck[[i]][order(DataToCheck[[i]]$Date, decreasing = FALSE),]
}


DivergenceSig<-function(Data){  #this function gives the divergence signal
  DivSig<-MACDPower(DataToBeTested=Data, ScheduleAlert=TRUE)
  
  value<-sum(DivSig[[2]][,1])+sum(DivSig[[3]][,1])+sum(DivSig[[4]][,1])+sum(DivSig[[5]][,1])
  
  BOLLvalue<-0
  if (DivSig[[6]][,2]>=0.75 | DivSig[[6]][,2]<=0.25){BOLLvalue<-1}
  
  CandleRank<-DivSig[[7]][,1] #this is 启动K线排名
  StarStrength<-DivSig[[7]][,2] #this is 分型强度 in MACD
  MACDalert<-DivSig[[7]][,3] #this is MACD Alert
  
  Signal<-0
  if(value>=4 & StarStrength>0 & (CandleRank!=0 | MACDalert==1) ){Signal<-1}
  
  return(Signal)
}


AutoCheck<-function(PriceData){
  SBPStr<-ChanLunStr(PriceData)
  Bi<-SBPStr$Bi
  Pivotalplanet<-tail(SBPStr$BiPlanetStr,1)
  
  OutPlanetBi<-subset(Bi, Bi$BiStartD==Pivotalplanet$PlanetEndD) 

  res5<-FuncEMA5(PriceData)
  res20<-FuncEMA20(PriceData)
  res30<-FuncEMA30(PriceData)
  res60<-FuncEMA60(PriceData)
  
  Signal<-DivergenceSig(PriceData)
  
  if(OutPlanetBi$SLOPE==1){
    Profit<-ifelse(last(res5$EMA5)<last(res20$EMA20), 1, 0)
    Alert<-ifelse(last(res20$EMA20)<last(res60$EMA60) | last(res30$EMA30)<last(res60$EMA60), 1, 0)
  }else{
    Profit<-0
    Alert<-0}
  
  return(list(Signal=Signal, Profit=Profit, Alert=Alert))
}


Results<-AutoCheck(DataToCheck[[1]])

#the divergence signal alertbeep(1)
if (Results$Signal==1){for(i in 1:3){beep(1); Sys.sleep(1)}}

#the profit signal alertbeep(2)
if (Results$Profit==1){for(i in 1:3){beep(2); Sys.sleep(1)}}

#the reversal alert signal alertbeep(3)
if (Results$Alert==1){beep(3); Sys.sleep(4)}


