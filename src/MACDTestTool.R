MACDTest<-function(Pricedata, Title, CheckReversal=FALSE){
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
    result<-MACDPower(DataToBeTested=subset(Pricedata,Date<=Pdate),Title=Title,BarOverride=FALSE,SBPStr=SBPStr,Data_macd=Data_macd, Data_mfi=Data_mfi,Data_boll=Data_boll)
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


MACDThreeLineTest<-function(CombData, specifyDate=NULL){
  Data_macd<-PricedataMACD(CombData) #calculate the MACD
  Data_MF<-PricedataMoneyFlow(CombData)
  Data_MFI<-PricedataMFI(CombData)
  Data_EMA60<-FuncEMA60(CombData)
  
  SBPStr<-ChanLunStr(CombData)
  Bi<-SBPStr$Bi
  
  i=ifelse(is.null(specifyDate), nrow(Bi)-3, which(Bi$BiStartD==specifyDate))
  
  while(i>=1){
    if(i>(nrow(Bi)-3)){stop("Choose a specifyDate that has at least 4 Bi.")}
    else if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i]){
      start_ind=Bi$BiStartD[i]
      
      BreakoutStructure=list(Bi=filter(Bi,BiStartD>=start_ind), 
                             Price=filter(CombData,Date>=start_ind),
                             MACD=filter(Data_macd,Date>=start_ind),
                             MF=filter(Data_MF,Date>=start_ind),
                             MFI=filter(Data_MFI,Date>=start_ind)
      )
      
      #check MACD reversal
      macd_rev1=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      macd_rev1_bygroup=macd_rev1%>%split_interval()#group the macd into pos and neg values
      macd_rev2=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
          
      mf_rev1=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      mf_rev2=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
      maxMF=list(mf_rev1,mf_rev2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = TRUE)[1:3]))


      
      #check MACD divergence
      macd_div1 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
      macd_div2 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      
      #假跌破
      falsebreakout=ifelse((min(macd_div2$MACD) < 1.3*min(macd_div1$MACD)) & 
                             (max(macd_rev2$MACD) > 1.3*max(macd_rev1$MACD)) &
                             (max(mf_rev2$MoneyFlow) >= max(mf_rev1$MoneyFlow)), 1, 0)
      
      mf_div1 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
      mf_div2 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      mfi_div1 <- subset(BreakoutStructure$MFI, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
      mfi_div2 <- subset(BreakoutStructure$MFI, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      
      minMF<-list(mf_div1,mf_div2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = FALSE)[1:3]))
      minMFI<-list(mfi_div1,mfi_div2)%>%map(~min(.x$MFI))
      
      if(falsebreakout==1){div=1}
      else if((min(macd_div2$MACD) > min(macd_div1$MACD)) &
              ((0.999*minMF[[2]]>minMF[[1]]) | (minMFI[[2]]>minMFI[[1]]))
      ){div=1}else{div=0}
      
      
      
      res <- data.frame(
        Key = c(
          "MACD1", "MACD2", 
          "MF1", "MF2", 
          "MFI1", "MFI2", 
          "Period Start", "Period End", 
          "Divergence", "False Breakout"
        ),
        Value = c(
          min(macd_div1$MACD), min(macd_div2$MACD), 
          minMF[[1]], 0.999*minMF[[2]], 
          minMFI[[1]], minMFI[[2]], 
          BreakoutStructure$Bi[1, "BiStartD"], 
          BreakoutStructure$Bi[1 + 2, "BiEndD"],
          as.logical(div), falsebreakout
        ),
        stringsAsFactors = FALSE
      )
      
      # Print the data frames
      cat("\nDivergence Information:\n")
      print(res)
      
      
      
      break
    }
    else{i=i-1}
  }
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
