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



##### NQ
#"2020-03-20","2020-06-19","2020-09-18","2020-12-18","2021-03-19","2021-06-18","2021-09-17","2021-12-17",

NQExpD <- c("2023-03-17") #this is the expiry date of the contract, can be found in IB Description

for(i in 1:length(NQExpD)){
  exp <- format(as.Date(NQExpD[i]), "%Y%m%d")
  endtime<-paste(exp, "00:00:00")
  NQtitle<-paste0("NQ30F_", exp)
  fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
  
  twsNQ <- twsFuture(symbol = "NQ",exch="CME", expiry=exp, currency="USD", multiplier = "20", include_expired="1")
  NQ<- reqHistoricalData(tws, Contract=twsNQ, endDateTime=endtime, barSize='30 mins', duration='4 M', useRTH='0', whatToShow='TRADES')
  write.zoo(NQ, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
  assign(NQtitle,NQ)
  #Sys.sleep(20)
}


FUT_NQ1<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20200320.csv"),header=T) 
FUT_NQ2<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20200619.csv"),header=T) 
FUT_NQ3<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20200918.csv"),header=T) 
FUT_NQ4<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20201218.csv"),header=T) 
FUT_NQ5<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20210319.csv"),header=T) 
FUT_NQ6<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20210618.csv"),header=T) 
FUT_NQ7<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20210917.csv"),header=T) 
FUT_NQ8<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20211217.csv"),header=T) 
FUT_NQ9<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20220318.csv"),header=T) 
FUT_NQ10<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20220617.csv"),header=T) 
FUT_NQ11<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20220916.csv"),header=T) 
FUT_NQ12<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20221216.csv"),header=T) 
FUT_NQ13<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20230317.csv"),header=T) 


FUT_NQ1<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20200320.csv"),header=T) 
FUT_NQ2<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20200619.csv"),header=T) 
FUT_NQ3<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20200918.csv"),header=T) 
FUT_NQ4<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20201218.csv"),header=T) 
FUT_NQ5<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20210319.csv"),header=T) 
FUT_NQ6<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20210618.csv"),header=T) 
FUT_NQ7<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20210917.csv"),header=T) 
FUT_NQ8<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20211217.csv"),header=T) 
FUT_NQ9<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20220318.csv"),header=T) 
FUT_NQ10<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20220617.csv"),header=T) 
FUT_NQ11<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20220916.csv"),header=T) 
FUT_NQ12<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20221216.csv"),header=T) 
FUT_NQ13<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20230317.csv"),header=T) 


FUT_NQ1<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20200320.csv"),header=T) 
FUT_NQ2<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20200619.csv"),header=T) 
FUT_NQ3<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20200918.csv"),header=T) 
FUT_NQ4<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20201218.csv"),header=T) 
FUT_NQ5<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20210319.csv"),header=T) 
FUT_NQ6<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20210618.csv"),header=T) 
FUT_NQ7<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20210917.csv"),header=T) 
FUT_NQ8<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20211217.csv"),header=T) 
FUT_NQ9<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20220318.csv"),header=T) 
FUT_NQ10<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20220617.csv"),header=T) 
FUT_NQ11<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20220916.csv"),header=T) 
FUT_NQ12<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20221216.csv"),header=T) 
FUT_NQ13<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/NQ/Continuous/NQ1H_20230317.csv"),header=T) 


FUT_NQ1$FND<-tail(FUT_NQ1$Index,1)
FUT_NQ2$FND<-tail(FUT_NQ2$Index,1)
FUT_NQ3$FND<-tail(FUT_NQ3$Index,1)
FUT_NQ4$FND<-tail(FUT_NQ4$Index,1)
FUT_NQ5$FND<-tail(FUT_NQ5$Index,1)
FUT_NQ6$FND<-tail(FUT_NQ6$Index,1)
FUT_NQ7$FND<-tail(FUT_NQ7$Index,1)
FUT_NQ8$FND<-tail(FUT_NQ8$Index,1)
FUT_NQ9$FND<-tail(FUT_NQ9$Index,1)
FUT_NQ10$FND<-tail(FUT_NQ10$Index,1)
FUT_NQ11$FND<-tail(FUT_NQ11$Index,1)
FUT_NQ12$FND<-tail(FUT_NQ12$Index,1)
FUT_NQ13$FND<-tail(FUT_NQ13$Index,1)

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


head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file=paste0(getwd(),"/Data/OriginalFuturesData/NQ/NQ30FContinuous.csv"), row.names = FALSE) 


