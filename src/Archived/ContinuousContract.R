##################################################################Archived
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


FUT_GC1<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20190828.csv"),header=T) 
FUT_GC2<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20190926.csv"),header=T)
FUT_GC3<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20191029.csv"),header=T)
FUT_GC4<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20191126.csv"),header=T)
FUT_GC5<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20191227.csv"),header=T)
FUT_GC6<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200129.csv"),header=T)
FUT_GC7<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200226.csv"),header=T)
FUT_GC8<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200327.csv"),header=T)
FUT_GC9<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200428.csv"),header=T)
FUT_GC10<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200527.csv"),header=T)
FUT_GC11<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200626.csv"),header=T)
FUT_GC12<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200729.csv"),header=T)
FUT_GC13<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200827.csv"),header=T)
FUT_GC14<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20200928.csv"),header=T)
FUT_GC15<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20201028.csv"),header=T)
FUT_GC16<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20201125.csv"),header=T)
FUT_GC17<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20201229.csv"),header=T)
FUT_GC18<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210127.csv"),header=T)
FUT_GC19<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210224.csv"),header=T)
FUT_GC20<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210329.csv"),header=T)
FUT_GC21<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210428.csv"),header=T)
FUT_GC22<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210526.csv"),header=T)
FUT_GC23<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210628.csv"),header=T)
FUT_GC24<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210728.csv"),header=T)
FUT_GC25<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210827.csv"),header=T)
FUT_GC26<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20210928.csv"),header=T)
FUT_GC27<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20211027.csv"),header=T)
FUT_GC28<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20211126.csv"),header=T)
FUT_GC29<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/GC/Continuous/GC_20211229.csv"),header=T)

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


ContinuousFut<-CombineContracts(FUT_GC1, FUT_GC5)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC7)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC9)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC11)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC13)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC17)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC19)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC21)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC23)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC25)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_GC29)
head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file=paste0(getwd(), "/Data/OriginalFuturesData/GC/GCContinuous.csv"), row.names = FALSE) 



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


FUT_CAD1<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20191217.csv"),header=T) 
FUT_CAD2<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200317.csv"),header=T) 
FUT_CAD3<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200616.csv"),header=T) 
FUT_CAD4<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20200915.csv"),header=T) 
FUT_CAD5<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20201215.csv"),header=T) 
FUT_CAD6<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210316.csv"),header=T) 
FUT_CAD7<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210615.csv"),header=T) 
FUT_CAD8<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20210914.csv"),header=T) 
FUT_CAD9<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20211214.csv"),header=T) 
FUT_CAD10<-read.csv(file = paste0(getwd(), "/Data/OriginalFuturesData/CAD/Continuous/CAD_20220315.csv"),header=T) 

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

ContinuousFut<-CombineContracts(FUT_CAD1, FUT_CAD2)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD3)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD4)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD5)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD6)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD7)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD8)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD9)
ContinuousFut<-CombineContracts(ContinuousFut, FUT_CAD10)

head(ContinuousFut,30)
tail(ContinuousFut,30)

write.csv(ContinuousFut, file=paste0(getwd(), "/Data/OriginalFuturesData/CAD/CADContinuous.csv"), row.names = FALSE) 
