MACDCalculatorOld<-function(Pricedata, MACDType, Period, StarData, LineDivPeriod){
  source("/Users/tengli/R/Script/StockPlotFunction.R") #everytime we run a function from a different script, we must run this command
  
  if(MACDType=="MACD"){Data_macd<-PricedataMACD(Pricedata)}else{Data_macd <- VMACD(Pricedata)}
  LDiv1Length<-which(Data_macd$Date==LineDivPeriod[1,"BiEndD"])-which(Data_macd$Date==LineDivPeriod[1,"BiStartD"])
  LDiv2Length<-which(Data_macd$Date==LineDivPeriod[2,"BiEndD"])-which(Data_macd$Date==LineDivPeriod[2,"BiStartD"])
  LDivWeight<-LDiv2Length/(LDiv1Length+LDiv2Length)  
  
  A1_interval <- subset(Data_macd,Date>= Period$In1 & Date<=Period$In2)
  A1_length<-which(A1_interval$Date==Period$In2)-which(A1_interval$Date==Period$In1)
  area_neg1 <- sum(A1_interval[which(A1_interval[,3]<0),3])/A1_length
  area_pos1 <- sum(A1_interval[which(A1_interval[,3]>=0),3])/A1_length
  
  A2_interval <- subset(Data_macd,Date>=Period$Out1 & Date<=Period$Out2)
  A2_length<-which(A2_interval$Date==Period$Out2)-which(A2_interval$Date==Period$Out1)
  area_neg2 <- sum(A2_interval[which(A2_interval[,3]<0),3])/A2_length
  area_pos2 <- sum(A2_interval[which(A2_interval[,3]>=0),3])/A2_length
  
  LineDiv1MACD<-1*sum(abs(Data_macd[which(Data_macd$Date>=LineDivPeriod[1,"BiStartD"] & Data_macd$Date<=LineDivPeriod[1,"BiEndD"]),]$MACD))
  LineDiv2MACD<-0.5*exp(LDivWeight^2)*sum(abs(Data_macd[which(Data_macd$Date>=LineDivPeriod[2,"BiStartD"] & Data_macd$Date<=LineDivPeriod[2,"BiEndD"]),]$MACD))
  
  Planet_interval<-subset(Data_macd,Date>= Period$In2 & Date<=Period$Out2)
  
  StarMACDArea<-sum(tail(A2_interval,2)$MACD)
  StarType<-StarData[which(StarData$Date==Period$Out2),"StarType"]
  StarBeginDate<-tail(A2_interval,2)$Date[1]
  StarBeginHigh<-Pricedata[which(Pricedata$Date==StarBeginDate),"High"]
  StarBeginLow<-Pricedata[which(Pricedata$Date==StarBeginDate),"Low"]
  
  CountIndex<-1
  StopCount<-0
  while(CountIndex>0 & CountIndex<=(nrow(Pricedata)-which(Pricedata$Date==Period$Out2)) & StopCount==0){
    if((StarType==2 & Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Close"]>=StarBeginHigh) |
       (StarType==1 & Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Close"]<=StarBeginLow)){
      OpenPositionSignalDate<-Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Date"]
      StopCount<-1
      break
    }else{CountIndex<-CountIndex+1}
  }
  
  OpenPositionSignal<-function(){
    if(StopCount==1){
      SignalMacd<-Data_macd[which(Data_macd$Date==OpenPositionSignalDate),"MACD"]
      
      if((StarMACDArea>0 & SignalMacd>StarMACDArea) |
         (StarMACDArea<0 & SignalMacd<StarMACDArea)){return((SignalMacd-StarMACDArea)/(sqrt(CountIndex)*StarMACDArea))}
      else{return(-(SignalMacd-StarMACDArea)/(sqrt(CountIndex)*StarMACDArea))}
      
    }else{return(0)}
  }
  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 4)
  colnames(DivergenceMatrix) <-c("背驰", "强度", "穿零轴","分型强度")
  if(MACDType=="MACD"){
    if(Pricedata[which(Pricedata$Date==as.character(Period$In1)),]$High<Pricedata[which(Pricedata$Date==as.character(Period$Out2)),]$High){
      rownames(DivergenceMatrix)<-c("MACD 上涨能量背驰")      
      DivergenceMatrix["MACD 上涨能量背驰","强度"]<-(1-area_pos2/area_pos1)      
      DivergenceMatrix["MACD 上涨能量背驰","穿零轴"]<-any(Planet_interval$DIFF<=0 | Planet_interval$DEA<=0)
      DivergenceMatrix["MACD 上涨能量背驰","背驰"]<-(DivergenceMatrix["MACD 上涨能量背驰","强度"]>=0.6)
      DivergenceMatrix["MACD 上涨能量背驰","分型强度"]<-OpenPositionSignal()
    }else{
      rownames(DivergenceMatrix)<-c("MACD 下跌能量背驰")      
      DivergenceMatrix["MACD 下跌能量背驰","强度"]<-(1-abs(area_neg2)/abs(area_neg1))
      DivergenceMatrix["MACD 下跌能量背驰","穿零轴"]<-any(Planet_interval$DIFF>=0 | Planet_interval$DEA>=0)
      DivergenceMatrix["MACD 下跌能量背驰","背驰"]<-(DivergenceMatrix["MACD 下跌能量背驰","强度"]>=0.6)
      DivergenceMatrix["MACD 下跌能量背驰","分型强度"]<-OpenPositionSignal()
    }
  }#else{
  #if(Pricedata[which(Pricedata$Date==as.character(Period$In1)),]$High<Pricedata[which(Pricedata$Date==as.character(Period$Out2)),]$High){
  # rownames(DivergenceMatrix) <- c("Volume MACD 上涨能量背驰")
  #DivergenceMatrix["Volume MACD 上涨能量背驰","强度"]<-(1-abs(area_neg2)/abs(area_neg1))
  #DivergenceMatrix["Volume MACD 上涨能量背驰","背驰"]<-(DivergenceMatrix["Volume MACD 上涨能量背驰","强度"]>=0.6)
  #DivergenceMatrix["Volume MACD 上涨能量背驰","穿零轴"]<-0
  #}else{
  # rownames(DivergenceMatrix) <- c("Volume MACD 下跌能量背驰")
  #DivergenceMatrix["Volume MACD 下跌能量背驰","强度"]<-(1-area_pos2/area_pos1)
  #DivergenceMatrix["Volume MACD 下跌能量背驰","背驰"]<-(DivergenceMatrix["Volume MACD 下跌能量背驰","强度"]>=0.6)
  #DivergenceMatrix["Volume MACD 下跌能量背驰","穿零轴"]<-0
  #}
  #}
  
  ThreeLineDivMatrix<-rbind(data.frame(Date=LineDivPeriod[1,"BiStartD"], MACD=LineDiv1MACD),data.frame(Date=LineDivPeriod[2,"BiStartD"], MACD=LineDiv2MACD))
  ThreeLineDivMatrix<-cbind(ThreeLineDivMatrix, ThreeLineDivergence=as.numeric(LineDiv1MACD>LineDiv2MACD))
  return(list(DivergenceMatrix=DivergenceMatrix,ThreeLineDivMatrix=ThreeLineDivMatrix))
}


MFICalculatorOld<-function(Pricedata, Period){
  source("/Users/tengli/R/Script/StockPlotFunction.R") #everytime we run a function from a different script, we must run this command
  MFI<-PricedataMFI(Pricedata)
  
  A1_interval <- subset(MFI,Date>= Period$In1 & Date<=Period$In2)
  area_neg1 <- sum(A1_interval[which(A1_interval$MFI<0),2])
  area_pos1 <- sum(A1_interval[which(A1_interval$MFI>=0),2])
  
  A2_interval <- subset(MFI,Date>= Period$Out1 & Date<=Period$Out2)
  area_neg2 <- sum(A2_interval[which(A2_interval$MFI<0),2])
  area_pos2 <- sum(A2_interval[which(A2_interval$MFI>=0),2])
  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 2)
  colnames(DivergenceMatrix) <-c("背驰", "强度")
  
  if(Pricedata[which(Pricedata$Date==as.character(Period$In1)),]$High<Pricedata[which(Pricedata$Date==as.character(Period$Out2)),]$High){
    rownames(DivergenceMatrix)<-c("MFI 上涨背驰")      
    DivergenceMatrix["MFI 上涨背驰","强度"]<-(1-area_pos2/area_pos1)
    DivergenceMatrix["MFI 上涨背驰","背驰"]<-(DivergenceMatrix["MFI 上涨背驰","强度"]>=0.6)
  }else{
    rownames(DivergenceMatrix)<-c("MFI 下跌背驰")      
    DivergenceMatrix["MFI 下跌背驰","强度"]<-(1-abs(area_neg2)/abs(area_neg1))
    DivergenceMatrix["MFI 下跌背驰","背驰"]<-(DivergenceMatrix["MFI 下跌背驰","强度"]>=0.6)
  }
  return(DivergenceMatrix)
}

MACDPowerOld<-function(DataToBeTested, Period=NULL, BarOverride){
  source("/Users/tengli/R/Script/ChanLunFunction.R") #everytime we run a function from a different script, we must run this command
  StarData <- StarFunction(DataToBeTested)
  Bi<-BiFunction(StarData)
  Finalplanet <- as.data.frame(PlanetFunction(Bi))
  Finalplanet <- tail(subset(Finalplanet, PlanetHigh!=0),n=1)
  
  InPlanetBi<-subset(Bi, Bi$BiEndD==Finalplanet$PlanetStartD)
  OutPlanetBi<-subset(Bi, Bi$BiStartD==Finalplanet$PlanetEndD)
  
  OutIndex<-which(Bi$SLOPE==OutPlanetBi$SLOPE & Bi$BiStartD==OutPlanetBi$BiStartD)
  
  if(BarOverride==FALSE){
    In1<-InPlanetBi$BiStartD #this gives the start date of the incoming bi of the final planet
    In2<-InPlanetBi$BiEndD #this gives the end date of the incoming bi of the final planet
    Out1<-OutPlanetBi$BiStartD #this gives the start date of the leaving bi of the final planet
    
    
    if(OutPlanetBi$SLOPE==1 
       & OutIndex<=(nrow(Bi)-2) 
       & Bi[OutIndex+2,"MIN"]>=Finalplanet$PlanetHigh 
       & Bi[OutIndex+2,"MAX"]>=Bi[OutIndex,"MAX"]){
      Out2 <- Bi[OutIndex+2,"BiEndD"]
    }else if(OutPlanetBi$SLOPE== -1 
             & OutIndex<=(nrow(Bi)-2) 
             & Bi[OutIndex+2,"MAX"]<=Finalplanet$PlanetLow 
             & Bi[OutIndex+2,"MIN"]<=Bi[OutIndex,"MIN"]){
      Out2 <- Bi[OutIndex+2,"BiEndD"]
    }else{Out2<-OutPlanetBi$BiEndD}
    
    Period <- data.frame(In1,In2,Out1,Out2) #this gives the end date of the leaving bi of the final planet
  }
  
  LineDiv1<-Bi[nrow(Bi)-3,]
  LineDiv2<-Bi[nrow(Bi)-1,]
  LineDivPeriod<-rbind(LineDiv1,LineDiv2)[,c(4,5)]
  
  DivergenceList<-list()
  if (nrow(Finalplanet)!=0 | BarOverride==TRUE){
    MACDensemble<-MACDCalculatorOld(DataToBeTested, "MACD", Period, StarData, LineDivPeriod)
    DivergenceList[["Period"]]<-Period
    DivergenceList[["MACD"]]<-MACDensemble[["DivergenceMatrix"]]   #this gives the MACD
    DivergenceList[["MFI"]]<-MFICalculatorOld(DataToBeTested, Period)         #this gives the MFI
    
    Bar1length<-abs(StarData[which(StarData$Date==as.character(Period$In1)),]$Price-StarData[which(StarData$Date==as.character(Period$In2)),]$Price)    
    Bar2length<-abs(StarData[which(StarData$Date==as.character(Period$Out1)),]$Price-StarData[which(StarData$Date==as.character(Period$Out2)),]$Price)
    
    DivergenceList[["形态背驰"]]<-matrix(c(Bar2length<Bar1length,(1-Bar2length/Bar1length)), nrow=1, ncol=2, dimnames = list("形态背驰", c("背驰","强度")))
    
    BOLL<- BBands(Cl(DataToBeTested), n=60, sd=2, maType = EMA)
    BOLL<-na.omit(data.frame(Date=DataToBeTested$Date,PctB=BOLL[,"pctB"]))
    
    if(rownames(DivergenceList[["MACD"]])=="MACD 上涨能量背驰"){
      DivergenceList[["BOLL信号"]]<-matrix(c(1,BOLL[which(BOLL$Date==as.character(Period$Out2)),]$PctB), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }else{
      DivergenceList[["BOLL信号"]]<-matrix(c(-1,BOLL[which(BOLL$Date==as.character(Period$Out2)),]$PctB), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }
    
    #DivergenceList[["VMACD"]]<-MACDCalculator(DataToBeTested, "VMACD", Period)         #this gives the Volume MACD
    
    DivergenceList[["ThreeLineDiv"]]<-MACDensemble[["ThreeLineDivMatrix"]]   #this gives the MACD
    
  }else{print("There is no 中枢 to compare.")}
  return(DivergenceList)
}

CoDivergenceOld<-function(DataToBeTested, Period=NULL, BarOverride=FALSE){
  CoDivergenceList<-list()
  CoDivergenceDivergenceMatrix<-as.data.frame(matrix(0, nrow = length(DataToBeTested)+1, ncol=5), stringsAsFactors=F)
  colnames(CoDivergenceDivergenceMatrix) <-c("中枢方向","MACD背驰强度","MFI背驰强度","形态背驰强度","BOLL信号")
  for (i in 1:length(DataToBeTested)) {
    CoDivergenceList[i]<-list(MACDPower(DataToBeTested=DataToBeTested[[i]], Period=Period[[i]], BarOverride[i]))
    rownames(CoDivergenceDivergenceMatrix)[i]<-names(DataToBeTested)[i]
    
    if(rownames(CoDivergenceList[[i]]$MACD)=="MACD 上涨能量背驰"){
      CoDivergenceDivergenceMatrix[i,1]<-1}else{CoDivergenceDivergenceMatrix[i,1]<- -1
      }
    
    for (j in 2:4) {
      if(CoDivergenceList[[i]][[j]][,"背驰"]==1){
        CoDivergenceDivergenceMatrix[i,j]<-CoDivergenceList[[i]][[j]][,"强度"]
      }else{
        CoDivergenceDivergenceMatrix[i,j]<-0
      }
    }
    
    if (CoDivergenceList[[i]][[5]][,"强度"]>=0.75 | CoDivergenceList[[i]][[5]][,"强度"]<=0.25){
      CoDivergenceDivergenceMatrix[i,5]<-CoDivergenceList[[i]][[5]][,"强度"]
    }else{CoDivergenceDivergenceMatrix[i,5]<-0}
  }
  
  rownames(CoDivergenceDivergenceMatrix)[nrow(CoDivergenceDivergenceMatrix)]<-"总背驰"
  for(i in 1:5){CoDivergenceDivergenceMatrix[nrow(CoDivergenceDivergenceMatrix),i]<-sum(CoDivergenceDivergenceMatrix[,i])/length(DataToBeTested)}
  
  
  return(CoDivergenceDivergenceMatrix)
  
}