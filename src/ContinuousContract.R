library(tidyquant)
library(IBrokers)


######################################################### Import new data downloaded from IB_API, and make continous contract
#We use the backwards ratio method to adjust rollover. The rolling point chosen is the First Notice Date. For GC or HG, this date is 
#the last business day of the month prior to the contract month. If the business day coincide with the Bank Holiday, choose the previous
#business day. For example, GCJune2021, the business day is May31, but it's a holiday, so FND = May28.
#For 6C, rolling point is the Settlement Date. The Settlement Date is the Tuesday of the third week of the contract month.
#For equity index futures, rolling point is the Settlement Date. The Settlement Date is the Friday of the third week of the contract month.

#to find the contract details like conId, go to IB Watchlist, right click contract name and click Financial Instrument Info and select Detail
#see duration and barsize in https://interactivebrokers.github.io/tws-api/historical_limitations.html#pacing_violations

tws <- twsConnect(port = 7496) #to connect with TWS
isConnected(tws)#check if connected or not
twsConnectionTime(tws)# check what time did you connect
twsDisconnect(tws)#to disconnect

ContFUT <- function(OldF, NewF){
  colnames(OldF) <- c("Date", "Open", "High","Low", "Close", "Volume", "WAP", "GAPS", "Count", "FND")
  colnames(NewF) <- c("Date", "Open", "High","Low", "Close", "Volume", "WAP", "GAPS", "Count", "FND")
  RollPoint  <- unique(OldF$FND)
  AdjRate <- Cl(filter(NewF, Date==RollPoint))/Cl(filter(OldF, Date==RollPoint))
  OldF<- data.frame("Date"=OldF$Date, round(OldF[,c(2:5)]*AdjRate,5), OldF[,c(6:10)])
  
  VolAdj<-subset(merge(OldF[,c(1,6)], NewF[,c(1,6)], by= "Date"),Date<=RollPoint) #sum the volume of the two contracts that have overlaps before the rollpoint
  OldF[match(VolAdj$Date,OldF$Date,),]$Volume <- rowSums(VolAdj[,2:3])
  NewF[which(NewF$Date==RollPoint),]$Volume <- OldF[which(OldF$Date==RollPoint),]$Volume
  ContinuousFut <- rbind(filter(OldF, as.POSIXct(Date) < as.POSIXct(RollPoint)), filter(NewF, as.POSIXct(Date) >= as.POSIXct(RollPoint)))
  ContinuousFut$FND <- unique(NewF$FND)
  return(ContinuousFut)
}

##### GC
GCExpD <- c("2021-11-26","2021-12-29") 

for(i in 1:length(GCExpD)){
  exp <- format(as.Date(GCExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")   #if the current contract data is weird, try: format(Sys.time(), "%Y%m%d %H:%M:%S")
  GCtitle<-paste0("GC_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/GC/Continuous/", GCtitle, ".csv")
  
  twsGC <- twsFuture(symbol="GC",exch="COMEX", expiry=exp, currency="USD", multiplier = "100", include_expired="1")
  GC<- reqHistoricalData(tws, Contract=twsGC, endDateTime=endtime, barSize='1 day', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(GC, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(GCtitle,GC)
  Sys.sleep(60)
}


FUT_GC1<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20190828.csv",header=T) 
FUT_GC2<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20190926.csv",header=T)
FUT_GC3<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20191029.csv",header=T)
FUT_GC4<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20191126.csv",header=T)
FUT_GC5<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20191227.csv",header=T)
FUT_GC6<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200129.csv",header=T)
FUT_GC7<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200226.csv",header=T)
FUT_GC8<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200327.csv",header=T)
FUT_GC9<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200428.csv",header=T)
FUT_GC10<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200527.csv",header=T)
FUT_GC11<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200626.csv",header=T)
FUT_GC12<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200729.csv",header=T)
FUT_GC13<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200827.csv",header=T)
FUT_GC14<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20200928.csv",header=T)
FUT_GC15<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20201028.csv",header=T)
FUT_GC16<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20201125.csv",header=T)
FUT_GC17<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20201229.csv",header=T)
FUT_GC18<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210127.csv",header=T)
FUT_GC19<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210224.csv",header=T)
FUT_GC20<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210329.csv",header=T)
FUT_GC21<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210428.csv",header=T)
FUT_GC22<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210526.csv",header=T)
FUT_GC23<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210628.csv",header=T)
FUT_GC24<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210728.csv",header=T)
FUT_GC25<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210827.csv",header=T)
FUT_GC26<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20210928.csv",header=T)
FUT_GC27<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20211027.csv",header=T)
FUT_GC28<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20211126.csv",header=T)
FUT_GC29<-read.csv(file = "/Data/OriginalFuturesData/GC/Continuous/GC_20211229.csv",header=T)

FUT_GC1$FND <- "2019-07-31"
FUT_GC5$FND <- "2019-11-29"
FUT_GC7$FND <- "2020-01-31"
FUT_GC9$FND <- "2020-03-31"
FUT_GC11$FND <- "2020-05-29"
FUT_GC13$FND <- "2020-07-31"
FUT_GC17$FND <- "2020-11-30"
FUT_GC19$FND <- "2021-01-29"
FUT_GC21$FND <- "2021-03-31"
FUT_GC23$FND <- "2021-05-28"
FUT_GC25$FND <- "2021-07-30"
FUT_GC29$FND <- "2021-11-30"


ContinuousFut<-ContFUT(FUT_GC1, FUT_GC5)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC7)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC9)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC11)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC13)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC17)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC19)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC21)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC23)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC25)
ContinuousFut<-ContFUT(ContinuousFut, FUT_GC29)
head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file="/Data/OriginalFuturesData/GC/GCContinuous.csv", row.names = F) 


##### NQ
NQExpD <- c("2023-06-16") #this is the expiry date of the contract, can be found in IB Description

for(i in 1:length(NQExpD)){
  exp <- format(as.Date(NQExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")
  NQtitle<-paste0("NQ_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
  
  twsNQ <- twsFuture(symbol = "NQ",exch="CME", expiry=exp, currency="USD", multiplier = "20", include_expired="1")
  NQ<- reqHistoricalData(tws, Contract=twsNQ, endDateTime=endtime, barSize='1 day', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(NQ, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(NQtitle,NQ)
  #Sys.sleep(20)
}

#weekly:
FUT_NQ1<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20200320.csv",header=T) 
FUT_NQ2<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20200619.csv",header=T) 
FUT_NQ3<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20200918.csv",header=T) 
FUT_NQ4<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20201218.csv",header=T) 
FUT_NQ5<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20210319.csv",header=T) 
FUT_NQ6<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20210618.csv",header=T) 
FUT_NQ7<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20210917.csv",header=T) 
FUT_NQ8<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20211217.csv",header=T) 
FUT_NQ9<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20220318.csv",header=T) 
FUT_NQ10<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20220617.csv",header=T) 
FUT_NQ11<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20220916.csv",header=T) 
FUT_NQ12<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20221216.csv",header=T) 
FUT_NQ13<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20230317.csv",header=T) 
FUT_NQ14<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQW_20230616.csv",header=T) 

FUT_NQ1$FND<-"2020-03-20"
FUT_NQ2$FND<-"2020-06-19"
FUT_NQ3$FND<-"2020-09-18"
FUT_NQ4$FND<-"2020-12-18"
FUT_NQ5$FND<-"2021-03-19"
FUT_NQ6$FND<-"2021-06-18"
FUT_NQ7$FND<-"2021-09-17"
FUT_NQ8$FND<-"2021-12-17"
FUT_NQ9$FND<-"2022-03-18"
FUT_NQ10$FND<-"2022-06-17"
FUT_NQ11$FND<-"2022-09-16"
FUT_NQ12$FND<-"2022-12-16"
FUT_NQ13$FND<-"2023-03-17"
FUT_NQ14$FND<-"2023-06-16"

#daily
FUT_NQ1<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20190920.csv",header=T) 
FUT_NQ2<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20191220.csv",header=T) 
FUT_NQ3<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20200320.csv",header=T) 
FUT_NQ4<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20200619.csv",header=T) 
FUT_NQ5<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20200918.csv",header=T) 
FUT_NQ6<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20201218.csv",header=T) 
FUT_NQ7<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20210319.csv",header=T) 
FUT_NQ8<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20210618.csv",header=T) 
FUT_NQ9<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20210917.csv",header=T) 
FUT_NQ10<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20211217.csv",header=T) 
FUT_NQ11<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20220318.csv",header=T) 
FUT_NQ12<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20220617.csv",header=T) 
FUT_NQ13<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20220916.csv",header=T) 
FUT_NQ14<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20221216.csv",header=T) 
FUT_NQ15<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20230317.csv",header=T) 
FUT_NQ16<-read.csv(file = "/Data/OriginalFuturesData/NQ/Continuous/NQ_20230616.csv",header=T) 

FUT_NQ1$FND<-"2019-09-20"
FUT_NQ2$FND<-"2019-12-20"
FUT_NQ3$FND<-"2020-03-20"
FUT_NQ4$FND<-"2020-06-19"
FUT_NQ5$FND<-"2020-09-18"
FUT_NQ6$FND<-"2020-12-18"
FUT_NQ7$FND<-"2021-03-19"
FUT_NQ8$FND<-"2021-06-18"
FUT_NQ9$FND<-"2021-09-17"
FUT_NQ10$FND<-"2021-12-17"
FUT_NQ11$FND<-"2022-03-18"
FUT_NQ12$FND<-"2022-06-17"
FUT_NQ13$FND<-"2022-09-16"
FUT_NQ14$FND<-"2022-12-16"
FUT_NQ15$FND<-"2023-03-17"
FUT_NQ16$FND<-"2023-06-16"

ContinuousFut<-ContFUT(FUT_NQ1, FUT_NQ2)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ3)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ4)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ5)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ6)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ7)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ8)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ9)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ10)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ11)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ12)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ13)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ14)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ15)
ContinuousFut<-ContFUT(ContinuousFut, FUT_NQ16)

head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file="/Data/OriginalFuturesData/NQ/NQContinuous.csv", row.names = FALSE) 


#################### ES
ESExpD <- c("2021-09-17") 

for(i in 1:length(ESExpD)){
  exp <- format(as.Date(ESExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")
  EStitle<-paste0("ES_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/ES/Continuous/", EStitle, ".csv")
  
  twsES <- twsFuture(symbol = "ES",exch="CME", expiry=exp, currency="USD", multiplier = "50", include_expired="1")
  ES<- reqHistoricalData(tws, Contract=twsES, endDateTime=endtime, barSize='1 day', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(ES, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(EStitle,ES)
  Sys.sleep(60)
}



#################### YM
YMExpD <- c("2021-09-17") 

for(i in 1:length(YMExpD)){
  exp <- format(as.Date(YMExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")
  YMtitle<-paste0("YM_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/YM/Continuous/", YMtitle, ".csv")
  
  twsYM <- twsFuture(symbol = "YM",exch="ECBOT", expiry=exp, currency="USD", multiplier = "5", include_expired="1")
  YM<- reqHistoricalData(tws, Contract=twsYM, endDateTime=endtime, barSize='1 day', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(YM, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(YMtitle,YM)
  Sys.sleep(60)
}




#################### CAD
CADExpD <- c("2022-03-15")  #this is the expiry date of the contract, can be found in IB Description


for(i in 1:length(CADExpD)){
  exp <- format(as.Date(CADExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")
  CADtitle<-paste0("CAD_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/CAD/Continuous/", CADtitle, ".csv")
  
  twsCAD <- twsFuture(symbol = "CAD",exch="CME", expiry=exp, currency="USD", multiplier = "100000", include_expired="1")
  CAD<- reqHistoricalData(tws, Contract=twsCAD, endDateTime=endtime, barSize='1 day', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(CAD, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(CADtitle,CAD)
  #Sys.sleep(30)
}


FUT_CAD1<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20191217.csv",header=T) 
FUT_CAD2<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200317.csv",header=T) 
FUT_CAD3<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200616.csv",header=T) 
FUT_CAD4<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200915.csv",header=T) 
FUT_CAD5<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20201215.csv",header=T) 
FUT_CAD6<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210316.csv",header=T) 
FUT_CAD7<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210615.csv",header=T) 
FUT_CAD8<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210914.csv",header=T) 
FUT_CAD9<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20211214.csv",header=T) 
FUT_CAD10<-read.csv(file = "/Data/OriginalFuturesData/CAD/Continuous/CAD_20220315.csv",header=T) 

FUT_CAD1$FND<-"2019-12-17"
FUT_CAD2$FND<-"2020-03-17"
FUT_CAD3$FND<-"2020-06-16"
FUT_CAD4$FND<-"2020-09-15"
FUT_CAD5$FND<-"2020-12-15"
FUT_CAD6$FND<-"2021-03-16"
FUT_CAD7$FND<-"2021-06-15"
FUT_CAD8$FND<-"2021-09-14"
FUT_CAD9$FND<-"2021-12-14"
FUT_CAD10$FND<-"2022-03-15"

ContinuousFut<-ContFUT(FUT_CAD1, FUT_CAD2)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD3)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD4)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD5)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD6)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD7)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD8)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD9)
ContinuousFut<-ContFUT(ContinuousFut, FUT_CAD10)

head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file="/Data/OriginalFuturesData/CAD/CADContinuous.csv", row.names = F) 
