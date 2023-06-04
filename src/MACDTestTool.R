MACDTest<-function(Pricedata, CheckReversal=FALSE){
  SBPStr<-ChanLunStr(Pricedata)
  Data_macd<-PricedataMACD(Pricedata) #calculate the MACD
  Data_mfi<-PricedataMFI(Pricedata) #calculate the MFI
  Data_boll<-PricedataBOLL(Pricedata) #calculate the BOLL
  StarData<-SBPStr$StarData
  Bi<-SBPStr$Bi
  BiPlanetStr<-SBPStr$BiPlanetStr

  res<-function(BiEndD,OutIndex,Reversal1){
    if((which(StarData$Date==BiEndD)+2)<=nrow(StarData)){
      Pdate<-StarData[which(StarData$Date==BiEndD)+2,"Date"]
    }else{Pdate<-BiEndD}
    result<-MACDPower(DataToBeTested=subset(Pricedata,Date<=Pdate),BarOverride=FALSE,SBPStr=SBPStr,Data_macd=Data_macd, Data_mfi=Data_mfi,Data_boll=Data_boll)
    result[["SLOPE"]]<-result[["BOLL信号"]][1]
    if(OutIndex+3<=nrow(Bi)){
      Reversal<-ifelse(Reversal1==1,TRUE,FALSE)
    }else{Reversal<-"Unknown"}
    result[["Reversal"]]<-paste("Reverse",Reversal,sep = "")
    return(result)
  }
  
  result<-apply(BiPlanetStr,MARGIN = 1,FUN=function(x) res(BiEndD=x["BiEndD"], OutIndex=as.numeric(x["OutIndex"]), Reversal1=as.numeric(x["Reversal1"])))
  return(result)
}


MACDThreeLineTest<-function(Pricedata,LineDivPeriod){
  Data_macd<-PricedataMACD(Pricedata)
  LineDiv1MACD<-subset(Data_macd,Date>=LineDivPeriod[1,"BiStartD"] & Date<=LineDivPeriod[1,"BiEndD"])
  LineDiv2MACD<-subset(Data_macd,Date>=LineDivPeriod[2,"BiStartD"] & Date<=LineDivPeriod[2,"BiEndD"])
  
  #the range of time
  LDiv1Length<-nrow(LineDiv1MACD)
  LDiv2Length<-nrow(LineDiv2MACD)
  L_TimeWeight<-LDiv2Length/LDiv1Length
  
  #the range of price
  LDiv1Barlength<-abs(Pricedata[which(Pricedata$Date==LineDivPeriod[1,"BiEndD"]),]$Close-Pricedata[which(Pricedata$Date==LineDivPeriod[1,"BiStartD"]),]$Close)    
  LDiv2Barlength<-abs(Pricedata[which(Pricedata$Date==LineDivPeriod[2,"BiEndD"]),]$Close-Pricedata[which(Pricedata$Date==LineDivPeriod[2,"BiStartD"]),]$Close)
  L_PriceWeight<-LDiv2Barlength/LDiv1Barlength
  
  LDiveMeanWeight<-mean(c(L_TimeWeight,L_PriceWeight))
  
  #the MACD data for the lines considered
  Line_bar_1<-mean(abs(LineDiv1MACD$MACD))*LDiveMeanWeight #the smaller the value, the closer of two EMA lines in MACD, hence more likely to reverse
  Line_bar_2<-mean(abs(LineDiv2MACD$MACD))*LDiveMeanWeight
  
  Area<-as.numeric(Line_bar_1>Line_bar_2)
  return(Area)
}


MACDThreeLineHightTest<-function(Pricedata,LineDivPeriod){
  Data_macd<-PricedataMACD(Pricedata)
  LineDiv1MACD<-Data_macd[which(Data_macd$Date>=LineDivPeriod[1,"BiStartD"] & Data_macd$Date<=LineDivPeriod[1,"BiEndD"]),]
  LineDiv2MACD<-Data_macd[which(Data_macd$Date>=LineDivPeriod[2,"BiStartD"] & Data_macd$Date<=LineDivPeriod[2,"BiEndD"]),]
  
  Line_bar_1<-sum(abs(LineDiv1MACD$MACD)) #the smaller the value, the closer of two EMA lines in MACD, hence more likely to reverse
  Line_bar_2<-sum(abs(LineDiv2MACD$MACD))
  
  div<-as.numeric(Line_bar_1>Line_bar_2)
  return(div)
}

tempBi<-BiFunction(StarFunction(QQQ_daily))
ThreeLineTestResultCount<-0
ThreeLineHightTestResultCount<-0
for(i in 20:100){
  ThreeLineTestResult<-MACDThreeLineTest(QQQ_daily,LineDivPeriod=rbind(tempBi[i,], tempBi[i+2,])[,c(4,5)])
  ThreeLineHightTestResult<-MACDThreeLineHightTest(QQQ_daily,LineDivPeriod=rbind(tempBi[i,], tempBi[i+2,])[,c(4,5)])
  
  if (ThreeLineTestResult>0){ThreeLineTestResultCount<-ThreeLineTestResultCount+1}
  ThreeLineHightTestResultCount<-ThreeLineHightTestResultCount+ThreeLineHightTestResult
  
  print(rbind(tempBi[i,], tempBi[i+2,])[,c(4,5)])
  cat("New test: ", ThreeLineTestResult, "\n")
  cat("HightTest: ",ThreeLineHightTestResult, "\n")
}
cat("ThreeLineTestResultCount", ThreeLineTestResultCount)
cat("ThreeLineHightTestResultCount", ThreeLineHightTestResultCount)







A1<-subset(Data_macd1, Date>="2001-01-24" &Date<="2001-04-04" )
A2<-subset(Data_macd1, Date>="2001-08-02" &Date<="2001-09-21" )




A1<-subset(Data_macd1, Date>="2001-08-02" &Date<="2001-09-21" )
A2<-subset(Data_macd1, Date>="2002-05-14" &Date<="2002-07-02" )




A1<-subset(Data_macd1, Date>="2002-05-14" &Date<="2002-07-02" )
A2<-subset(Data_macd1, Date>="2002-08-22" &Date<="2002-10-07" )


A1<-subset(Data_macd1, Date>="2006-04-06" &Date<="2006-05-24" )
A2<-subset(Data_macd1, Date>="2006-06-30" &Date<="2006-07-14" )


A1<-subset(Data_macd1, Date>="2022-11-10 06:47:00" &Date<="2022-11-10 07:01:00" )
A2<-subset(Data_macd1, Date>="2022-11-10 08:14:00" &Date<="2022-11-10 08:29:00" )


Diff1<-(A1$MACD)
Diff1<-data.frame(x=index(Diff1), y=Diff1)

Diff2<-(A2$MACD)
Diff2<-data.frame(x=index(Diff2), y=Diff2)

plot(Diff1$x,Diff1$y,type = "l")
plot(Diff2$x,Diff2$y,type = "l")

Result<-lm(y~x,data = Diff1)
Result2<-lm(y~x,data = Diff2)
