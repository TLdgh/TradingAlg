library(beepr)
library(quantmod)
library(tidyverse)

source("/Users/tengli/R/TradingAlg/src/ChanLunFunction.R")
source("/Users/tengli/R/TradingAlg/src/StockPlotFunction.R") 
source("/Users/tengli/R/TradingAlg/src/MACDPower.R")

datalocation<-commandArgs(trailingOnly = TRUE)
write.table(datalocation,file=paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/OutputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE) # To be deleted

DataToCheck<-list()
for (i in 1:length(datalocation)){
  DataToCheck[[i]]<-read.csv(file=datalocation[i],header = TRUE)
  DataToCheck[[i]]<-DataToCheck[[i]][order(DataToCheck[[i]]$Date, decreasing = FALSE),]
}

#this is the initial Pivotalplanet setup
Pivotalplanet<-read.csv(file=paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/Pivotalplanet.csv"), header = TRUE)


DivergenceSig<-function(Data){  #this function gives the divergence signal
  DivSig<-MACDPower(DataToBeTested=Data, ScheduleAlert=TRUE)
  
  value<-sum(DivSig[[2]][,1])+sum(DivSig[[3]][,1])+sum(DivSig[[5]][,1])
  BOLLvalue<-0
  if (DivSig[[6]][,2]>=0.75 | DivSig[[6]][,2]<=0.25){BOLLvalue<-1}
  
  CandleRank<-DivSig[[7]][,1] #this is 启动K线排名
  StarStrength<-DivSig[[7]][,2] #this is 分型强度 in MACD
  MACDalert<-DivSig[[7]][,3] #this is MACD Alert
  
  Signal<-0
  if((value>=4 & DivSig$EMA[,"macd"]==1 & StarStrength>0) & (DivSig$EMA[,"Entanglement"]==1 | CandleRank!=0 | MACDalert==1) ){Signal<-1}
  else if(DivSig$EMA[,"macd"]==2 & StarStrength>0){Signal<-1}

  return(Signal)
}

TakeProfit<-function(Data){
  res1<-FuncEMA10(Data[(nrow(Data)-79):nrow(Data), ])
  res2<-FuncEMA30(Data[(nrow(Data)-79):nrow(Data), ])
  
  return(ifelse(last(res1$EMA10)<last(res2$EMA30), 1, 0))
}

TrendReverse<-function(PriceData,whichplanet=Pivotalplanet){    #check the alert signal for a planet, by default the latest planet 0
  SBPStr<-ChanLunStr(PriceData)
  Bi<-SBPStr$Bi
  
  InPlanetBi<-subset(Bi, Bi$BiEndD==whichplanet$PlanetStartD)
  OutPlanetBi<-subset(Bi, Bi$BiStartD==whichplanet$PlanetEndD)
  
  InIndex<-which(Bi$SLOPE==InPlanetBi$SLOPE & Bi$BiStartD==InPlanetBi$BiStartD)
  OutIndex<-which(Bi$SLOPE==OutPlanetBi$SLOPE & Bi$BiStartD==OutPlanetBi$BiStartD)
  
  beginl<-OutIndex
  Remain<-nrow(PriceData)-which(PriceData$Date==OutPlanetBi$BiEndD) #check #of data point remaining
  
  Signal<-0
  if(beginl<=(nrow(Bi)-1) & Remain>=3){
    Signal<-DivergenceSig(Data=subset(PriceData, index(PriceData)<=which(PriceData$Date==OutPlanetBi$BiEndD)+3))
  }
  
  Alert<-0
  while(beginl<=(nrow(Bi)-1) & Remain>=4){
    if (Bi[beginl,"SLOPE"]==1) {
      if(Bi[beginl+1, "MIN"]>Bi[beginl, "MIN"] & UpPlanetBreaker(Bi, beginl=InIndex, newhighline=nrow(Bi)+1)==0){
        if(beginl<=(nrow(Bi)-5) & Bi[beginl+3, "MIN"]<Bi[beginl+1, "MAX"] & SimpThirdSale(Bi=Bi,planet_range=planet(Bi,InIndex+1,InIndex+3),beginl=OutIndex)==0){    #if line2 and line4 has intersect and no 3rd sale to the original planet
          planet_range<-planet(Bi,beginl+1, beginl+3)
          count3rdsale<-0
          for (i in seq((beginl+5),nrow(Bi),2)) {
            if(Bi[i, "MAX"] < planet_range[1,2]){count3rdsale<-1} #if any future bi's max < planet low
          }
          if(count3rdsale==1){Alert<-1; break}else{break}
        }
        else if(beginl<=(nrow(Bi)-5) & SimpThirdSale(Bi=Bi,planet_range=planet(Bi,InIndex+1,InIndex+3),beginl=OutIndex)==1){
          Alert<-1;break
        }
        else{beginl<-beginl+2}
      }
      else if(Bi[beginl+1, "MIN"]<=Bi[beginl, "MIN"] & UpPlanetBreaker(Bi, beginl=InIndex, newhighline=nrow(Bi)+1)==0){
        planet_range<-planet(Bi,InIndex+1, InIndex+3)
        count3rdsale<-0
        for (i in seq((beginl),nrow(Bi)-1,2)) {
          if(Bi[i, "MAX"] < planet_range[1,2] | Bi[i,"MAX"]<Bi[beginl,"MIN"] ){count3rdsale<-1} #if any future bi's max < planet low or homo-thirdsale
        }
        if(count3rdsale==1){Alert<-1; break}else{break}
      }
      else if(UpPlanetBreaker(Bi, beginl=InIndex, newhighline=nrow(Bi)+1)!=0){Alert<-1; break}else{break}
    }
    else if (Bi[beginl,"SLOPE"]==-1) {
      if(Bi[beginl+1, "MAX"]<Bi[beginl, "MAX"] & DownPlanetBreaker(Bi, beginl=InIndex, newlowline=nrow(Bi)+1)==0){
        if(beginl<=(nrow(Bi)-5) & Bi[beginl+3, "MAX"]>Bi[beginl+1, "MIN"] & SimpThirdBuy(Bi=Bi,planet_range=planet(Bi,InIndex+1,InIndex+3),beginl=OutIndex)==0){             #if line2 and line4 has intersect
          planet_range<-planet(Bi,beginl+1, beginl+3)
          count3rdbuy<-0
          for (i in seq((beginl+5),nrow(Bi),2)) {
            if(Bi[i, "MIN"] > planet_range[1,1]){count3rdbuy<-1} #if any future bi's min > planet high
          }
          if(count3rdbuy==1){Alert<-1; break}else{break}
        }
        else if(beginl<=(nrow(Bi)-5) & SimpThirdBuy(Bi=Bi,planet_range=planet(Bi,InIndex+1,InIndex+3),beginl=OutIndex)==1){
          Alert<-1;break
        }
        else{beginl<-beginl+2}
      }
      else if(Bi[beginl+1, "MAX"]>=Bi[beginl, "MAX"] & DownPlanetBreaker(Bi, beginl=InIndex, newlowline=nrow(Bi)+1)==0){
        planet_range<-planet(Bi,InIndex+1, InIndex+3)
        count3rdbuy<-0
        for (i in seq((beginl),nrow(Bi)-1,2)) {
          if(Bi[i, "MIN"]>planet_range[1,1] | Bi[i,"MIN"]>Bi[beginl,"MAX"] ){count3rdbuy<-1}   #if any future bi's min > planet high or homo-thirdbuy
        }
        if(count3rdbuy==1){Alert<-1; break}else{break}
      }
      else if(DownPlanetBreaker(Bi, beginl=InIndex, newlowline=nrow(Bi)+1)!=0){Alert<-1; break}else{break}
    }
  }
  return(c(Alert,Signal))  #combine both the reversal alert and divergence signal
}

CheckPivotalplanet<-function(PriceData,Pivotalplanet){ #check if the next planet is of the same direction
  SBPStr<-ChanLunStr(PriceData)
  Bi<-SBPStr$Bi
  NewPivotalplanet<-tail(SBPStr$BiPlanetStr,1)
  NewInPlanetBi<-subset(Bi, Bi$BiEndD==NewPivotalplanet$PlanetStartD)
  OldInPlanetBi<-subset(Bi, Bi$BiEndD==Pivotalplanet$PlanetStartD)
  res<-ifelse(NewInPlanetBi$SLOPE==OldInPlanetBi$SLOPE,TRUE,FALSE)
  return(res)
}


NewPivotalplanet<-Pivotalplanet #setup new one the same way as the old one.
for (i in 1:length(DataToCheck)){
  if(CheckPivotalplanet(PriceData=DataToCheck[[i]],Pivotalplanet[i,])==TRUE){ #if true, update the pivotalplanet
    NewPivotalplanet[i,]<-tail(ChanLunStr(DataToCheck[[i]])$BiPlanetStr,1)%>%select(PlanetHigh:PlanetEndD)
  }
}
#save the NewPivotalplanet as the current one. Note if the check above is false, then this save won't change anything.
write.csv(NewPivotalplanet,file=paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/Pivotalplanet.csv"),row.names = FALSE)
#update the initial Pivotalplanet
Pivotalplanet<-NewPivotalplanet


RevOrDiv<-TrendReverse(PriceData=DataToCheck[[i]],whichplanet=Pivotalplanet[i,])
if (RevOrDiv[1]==1){ #the reversal alert
  for(j in 1:3){
    beep(1)
    Sys.sleep(1)
  }
}

if(RevOrDiv[2]==1){ #the divergence signal
  for(j in 1:1){
    beep(2)
    Sys.sleep(1)
  }
}  

#Profit taker function. Activate it when needed
if(FALSE){
  if(TakeProfit(DataToCheck[[i]])==1){ #the divergence signal
    for(j in 1:3){
      beep(3)
      Sys.sleep(4)
    }
  }
}  