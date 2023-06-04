MACDCalculator<-function(Pricedata, MACDType, Period, StarData, LineDivPeriod, ScheduleAlert=FALSE){
  source("C:\\R\\Script\\StockPlotFunction_Candy.R") #everytime we run a function from a different script, we must run this command
  
  ####################################################################################################
  ####--------------------------------First get the total MACD data--------------------------------###
  ####################################################################################################
  if(MACDType=="MACD"){Data_macd<-PricedataMACD(Pricedata)}else{Data_macd <- VMACD(Pricedata)}
  
  
  ####################################################################################################
  ####----Get the MACD area and reduction factor for the planet in order to consider divergence----###
  ####################################################################################################
  #calculate MACD of area 1 and 2
  A1_interval <- subset(Data_macd,Date>=Period$In1 & Date<=Period$In2)
  A2_interval <- subset(Data_macd,Date>=Period$Out1 & Date<=Period$Out2)
  A3_interval <- subset(Data_macd,Date>=Period$In2 & Date<=Period$Out1)
  
  #the range of time
  A1_length<-nrow(A1_interval)
  A2_length<-nrow(A2_interval)
  A_TimeWeight<-A2_length/A1_length
  
  #the range of price
  Bar1length<-abs(StarData[which(StarData$Date==Period$In1),]$Price-StarData[which(StarData$Date==Period$In2),]$Price)    
  Bar2length<-abs(StarData[which(StarData$Date==Period$Out1),]$Price-StarData[which(StarData$Date==Period$Out2),]$Price)
  A_PriceWeight<-Bar2length/Bar1length
  
  MeanWeight<-mean(A_TimeWeight,A_PriceWeight)
  
  if(StarData[which(StarData$Date==Period$In1),]$Price<StarData[which(StarData$Date==Period$In2),]$Price){ #price is going up
    Direction<-1
    area_pos1 <- A1_interval[which(A1_interval[,3]>0),3]
    area_pos2 <- A2_interval[which(A2_interval[,3]>0),3]
    if(length(area_pos1)>0){area_pos1<-mean(area_pos1)}else{area_pos1<-0}
    if(length(area_pos2)>0){area_pos2<-mean(area_pos2)*MeanWeight}else{area_pos2<-0}
    if(area_pos1!=0){Strength_pos<- 1-area_pos2/area_pos1}else{Strength_pos<-0}
    
    EMALine_Pos_1<-A1_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    EMALine_Pos_2<-A2_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    EMALine_Pos_3<-A3_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    if(nrow(EMALine_Pos_1)>0){MeanPos1<-mean(rowMeans(EMALine_Pos_1)); EMALine_Pos_1<-sum(rowMeans(EMALine_Pos_1))}else{MeanPos1<-0; EMALine_Pos_1<-0}
    if(nrow(EMALine_Pos_2)>0){MeanPos2<-mean(rowMeans(EMALine_Pos_2)); EMALine_Pos_2<-sum(rowMeans(EMALine_Pos_2))}else{MeanPos2<-0; EMALine_Pos_2<-0}
    if(nrow(EMALine_Pos_3)>0){MeanPos3<-mean(rowMeans(EMALine_Pos_3))}else{MeanPos3<-0}
    
    if(MeanPos2==min(MeanPos1,MeanPos2,MeanPos3)){Pos_ratio<-1}else{Pos_ratio<-1-MeanPos2/max(MeanPos1,MeanPos2,MeanPos3)}
    if(EMALine_Pos_1!=0){EMALineStrength_Pos<- 1-EMALine_Pos_2/EMALine_Pos_1}else{EMALineStrength_Pos<- 0}
  }else{
    Direction<- -1
    area_neg1 <- A1_interval[which(A1_interval[,3]<=0),3]
    area_neg2 <- A2_interval[which(A2_interval[,3]<=0),3]
    if(length(area_neg1)>0){area_neg1<-mean(area_neg1)}else{area_neg1<-0}
    if(length(area_neg2)>0){area_neg2<-mean(area_neg2)*MeanWeight}else{area_neg2<-0}
    if(area_neg1!=0){Strength_neg<- 1-area_neg2/area_neg1}else{Strength_neg<-0}
    
    EMALine_Neg_1<-A1_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    EMALine_Neg_2<-A2_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    EMALine_Neg_3<-A3_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    if(nrow(EMALine_Neg_1)>0){MeanNeg1<-abs(mean(rowMeans(EMALine_Neg_1))); EMALine_Neg_1<-abs(sum(rowMeans(EMALine_Neg_1)))}else{MeanNeg1<-0; EMALine_Neg_1<-0}
    if(nrow(EMALine_Neg_2)>0){MeanNeg2<-abs(mean(rowMeans(EMALine_Neg_2))); EMALine_Neg_2<-abs(sum(rowMeans(EMALine_Neg_2)))}else{MeanNeg2<-0; EMALine_Neg_2<-0}
    if(nrow(EMALine_Neg_3)>0){MeanNeg3<-abs(mean(rowMeans(EMALine_Neg_3)))}else{MeanNeg3<-0}
    
    if(MeanNeg2==min(MeanNeg1,MeanNeg2,MeanNeg3)){Neg_ratio<-1}else{Neg_ratio<-1-MeanNeg2/max(MeanNeg1,MeanNeg2,MeanNeg3)}
    if(EMALine_Neg_1!=0){EMALineStrength_Neg<- 1-EMALine_Neg_2/EMALine_Neg_1}else{EMALineStrength_Neg<- 0}
  }
  
  
  
  ####################################################################################################
  ####---------------Get the MACD area for the ThreeLine model to consider divergence--------------###
  ####################################################################################################
  #calculate MACD of the ThreeLine model
  LineDiv1MACD<-subset(Data_macd,Date>=LineDivPeriod[1,"BiStartD"] & Date<=LineDivPeriod[1,"BiEndD"])
  LineDiv2MACD<-subset(Data_macd,Date>=LineDivPeriod[2,"BiStartD"] & Date<=LineDivPeriod[2,"BiEndD"])
  
  #the range of time
  LDiv1Length<-nrow(LineDiv1MACD)
  LDiv2Length<-nrow(LineDiv2MACD)
  L_TimeWeight<-LDiv2Length/LDiv1Length
  
  #the range of price
  LDiv1Barlength<-abs(StarData[which(StarData$Date==LineDivPeriod[1,"BiEndD"]),]$Price-StarData[which(StarData$Date==LineDivPeriod[1,"BiStartD"]),]$Price)    
  LDiv2Barlength<-abs(StarData[which(StarData$Date==LineDivPeriod[2,"BiEndD"]),]$Price-StarData[which(StarData$Date==LineDivPeriod[2,"BiStartD"]),]$Price)
  L_PriceWeight<-LDiv2Barlength/LDiv1Barlength
  
  LDiveMeanWeight<-mean(c(L_TimeWeight,L_PriceWeight))
  
  #the MACD data for the lines considered
  Line_bar_1<-mean(abs(LineDiv1MACD$MACD))*LDiveMeanWeight #the smaller the value, the closer of two EMA lines in MACD, hence more likely to reverse
  Line_bar_2<-mean(abs(LineDiv2MACD$MACD))*LDiveMeanWeight
  
  ####################################################################################################
  ####------------------Calculate if the price has crossed the zero-line of MACD-------------------###
  ####################################################################################################
  Planet_interval<-subset(Data_macd,Date>= Period$In2 & Date<=Period$Out2)
  
  StarMACDArea<-mean(A2_interval$MACD)
  StarBeginDate<-tail(A2_interval,2)$Date[1]
  StarBeginHigh<-Pricedata[which(Pricedata$Date==StarBeginDate),"High"]
  StarBeginLow<-Pricedata[which(Pricedata$Date==StarBeginDate),"Low"]
  
  
  ####################################################################################################
  ####----------------------------Calculate the OpenPositionSignal---------------------------------###
  ####################################################################################################
  CountIndex<-1
  StopCount<-0
  while(CountIndex>0 & CountIndex<=(nrow(Pricedata)-which(Pricedata$Date==Period$Out2)) & StopCount==0){
    if((Direction==-1 & Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Close"]>=StarBeginHigh) |
       (Direction==1 & Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Close"]<=StarBeginLow)){
      OpenPositionSignalDate<-Pricedata[which(Pricedata$Date==Period$Out2)+CountIndex,"Date"]
      StopCount<-1
      break
    }else{CountIndex<-CountIndex+1}
  }
  
  OpenPositionSignal<-function(){
    if(StopCount==1){
      SignalMacd<-Data_macd[which(Data_macd$Date==OpenPositionSignalDate),"MACD"]
      
      if(Direction==1){
        if(SignalMacd*StarMACDArea>0){return(-(SignalMacd-StarMACDArea)/max(abs(SignalMacd),abs(StarMACDArea)) )}
        else{return(-sign(SignalMacd-StarMACDArea)*abs(SignalMacd)/min(abs(SignalMacd),abs(StarMACDArea)) )}
      }else{
        if(SignalMacd*StarMACDArea>0){return((SignalMacd-StarMACDArea)/max(abs(SignalMacd),abs(StarMACDArea)) )}
        else{return(sign(SignalMacd-StarMACDArea)*abs(SignalMacd)/min(abs(SignalMacd),abs(StarMACDArea)) )}
      }
    }else{return(0)}
  }
  
  ####################################################################################################
  ####----------------------------Calculate the length divergence----------------------------------###
  ####################################################################################################
  StrcturalDiv<-mean(c((1-A_TimeWeight), (1-A_PriceWeight)))
  StructuralDivMatrix<-matrix(c(StrcturalDiv>0,StrcturalDiv), nrow=1, ncol=2, dimnames = list("形态背驰", c("背驰","强度")))
  
  ####################################################################################################
  ####------------------------------------Check schedule alert-------------------------------------###
  ####################################################################################################
  if (ScheduleAlert==TRUE){
    MACDalert<-0
    if (Direction==1){
      MinMACD<-min(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)
      if(MinMACD>min(Data_macd[which(Data_macd$Date>Period$Out2),"MACD"])){MACDalert<-1}
    }else{
      MaxMACD<-max(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)
      if(MaxMACD<max(Data_macd[which(Data_macd$Date>Period$Out2),"MACD"])){MACDalert<-1}
    }
  }
  
  ####################################################################################################
  ####---------------------------------------SUMMARY-----------------------------------------------###
  ####################################################################################################
  MacdMatrix<-matrix(0, nrow = 3, ncol = 2)
  colnames(MacdMatrix) <-c("背驰", "强度")
  if(MACDType=="MACD"){
    if(Direction==1){
      rownames(MacdMatrix)<-c("MACD 上涨能量背驰", "MACD 均线面积背驰", "MACD 均线面积比例")      
      MacdMatrix["MACD 上涨能量背驰","强度"]<-Strength_pos 
      MacdMatrix["MACD 上涨能量背驰","背驰"]<-(Strength_pos>0)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Pos
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALine_Pos_2<EMALine_Pos_1)
      
      MacdMatrix["MACD 均线面积比例","强度"]<-Pos_ratio
      MacdMatrix["MACD 均线面积比例","背驰"]<-(round(Pos_ratio,2)>=0.6)
    }else{
      rownames(MacdMatrix)<-c("MACD 下跌能量背驰", "MACD 均线面积背驰", "MACD 均线面积比例")      
      MacdMatrix["MACD 下跌能量背驰","强度"]<-Strength_neg
      MacdMatrix["MACD 下跌能量背驰","背驰"]<-(Strength_neg>0)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Neg
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALine_Neg_2<EMALine_Neg_1)
      
      MacdMatrix["MACD 均线面积比例","强度"]<-Neg_ratio
      MacdMatrix["MACD 均线面积比例","背驰"]<-(round(Neg_ratio,2)>=0.6)
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
  #MacdMatrix2<-matrix(c(A_VolRatio, MaxWeight, A_Prob_R, A_ReductionFactor),nrow = 1, byrow = TRUE)
  #colnames(MacdMatrix2)<-c("A_VolRatio", "MaxWeight", "A_Prob_R", "A_ReductionFactor")
  #MacdMatrix2<-rbind(MacdMatrix2,MacdMatrix2)
  #MacdMatrix<-cbind(MacdMatrix,MacdMatrix2)
  
  if (ScheduleAlert==TRUE){
    Other<-data.frame(C1=any(Planet_interval$DIFF>=0 | Planet_interval$DEA>=0),
                      C2=OpenPositionSignal(), C3=MACDalert)
    colnames(Other)<-c("穿零轴", "分型强度", "MACD Alert")
  }else{
    Other<-data.frame(C1=any(Planet_interval$DIFF>=0 | Planet_interval$DEA>=0),
                      C2=OpenPositionSignal())
    colnames(Other)<-c("穿零轴", "分型强度")
  }
  
  DivergenceMatrix<-list(MacdMatrix=MacdMatrix, Other=Other)
  
  ThreeLineDivMatrix<-rbind(data.frame(Date=LineDivPeriod[1,"BiStartD"], MACD_Bar_area=Line_bar_1),
                            data.frame(Date=LineDivPeriod[2,"BiStartD"],MACD_Bar_area=Line_bar_2))
  
  ThreeLineDivMatrix<-cbind(ThreeLineDivMatrix, ThreeLineDivergence=as.numeric(Line_bar_1>Line_bar_2))
  
  return(list(DivergenceMatrix=DivergenceMatrix,ThreeLineDivMatrix=ThreeLineDivMatrix, StructuralDivMatrix=StructuralDivMatrix))
}


MFICalculator<-function(Pricedata, StarData, Period){
  source("C:\\R\\Script\\StockPlotFunction_Candy.R") #everytime we run a function from a different script, we must run this command
  MFI<-PricedataMFI(Pricedata)
  
  
  
  A1_interval <- subset(MFI,Date>= Period$In1 & Date<=Period$In2)
  A2_interval <- subset(MFI,Date>= Period$Out1 & Date<=Period$Out2)
  
  #the range of price
  A1_length<-nrow(A1_interval)
  A2_length<-nrow(A2_interval)
  A_TimeWeight<-A2_length/A1_length
  
  #the range of price
  Bar1length<-abs(StarData[which(StarData$Date==Period$In1),]$Price-StarData[which(StarData$Date==Period$In2),]$Price)  
  Bar2length<-abs(StarData[which(StarData$Date==Period$Out1),]$Price-StarData[which(StarData$Date==Period$Out2),]$Price)
  A_PriceWeight<-Bar2length/Bar1length
  
  MeanWeight<-mean(c(A_TimeWeight,A_PriceWeight))
  
  if(StarData[which(StarData$Date==Period$In1),]$Price<StarData[which(StarData$Date==Period$In2),]$Price){ #price is going up
    Direction<-1
    
    area_pos1 <- A1_interval[which(A1_interval[,2]>0),2]
    area_pos2 <- A2_interval[which(A2_interval[,2]>0),2]
    if(length(area_pos1)>0){area_pos1<-mean(area_pos1)}else{area_pos1<-1}
    if(length(area_pos2)>0){area_pos2<-mean(area_pos2)*MeanWeight}else{area_pos2<-1}
    if(area_pos1!=0){Strength_pos<- 1-area_pos2/area_pos1}else{Strength_pos<-0}
  }else{
    Direction<- -1
    area_neg1 <- A1_interval[which(A1_interval[,2]<=0),2]
    area_neg2 <- A2_interval[which(A2_interval[,2]<=0),2]
    if(length(area_neg1)>0){area_neg1<-mean(area_neg1)}else{area_neg1<-1}
    if(length(area_neg2)>0){area_neg2<-mean(area_neg2)*MeanWeight}else{area_neg2<-1}
    if(area_neg1!=0){Strength_neg<- 1-area_neg2/area_neg1}else{Strength_neg<-0}
  }
  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 2)
  colnames(DivergenceMatrix) <-c("背驰", "强度")
  
  if(Direction==1){
    rownames(DivergenceMatrix)<-c("MFI 上涨背驰")      
    DivergenceMatrix["MFI 上涨背驰","强度"]<-(Strength_pos)
    DivergenceMatrix["MFI 上涨背驰","背驰"]<-(Strength_pos>0)
  }else{
    rownames(DivergenceMatrix)<-c("MFI 下跌背驰")      
    DivergenceMatrix["MFI 下跌背驰","强度"]<-(Strength_neg)
    DivergenceMatrix["MFI 下跌背驰","背驰"]<-(Strength_neg>0)
  }
  return(DivergenceMatrix)
}

MACDPower<-function(DataToBeTested, Period=NULL, BarOverride, ScheduleAlert=FALSE){
  source("C:\\R\\Script\\ChanLunFunction_Candy.R") #everytime we run a function from a different script, we must run this command
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
    MACDensemble<-MACDCalculator(DataToBeTested, "MACD", Period, StarData, LineDivPeriod, ScheduleAlert)
    DivergenceList[["Period"]]<-Period
    DivergenceList[["MACD"]]<-MACDensemble[["DivergenceMatrix"]][["MacdMatrix"]]   #this gives the MACD
    DivergenceList[["MFI"]]<-MFICalculator(DataToBeTested, StarData, Period)         #this gives the MFI
    
    DivergenceList[["形态背驰"]]<-MACDensemble[["StructuralDivMatrix"]]
    
    BOLL<- BBands(Cl(DataToBeTested), n=60, sd=2, maType = EMA)
    BOLL<-na.omit(data.frame(Date=DataToBeTested$Date,PctB=BOLL[,"pctB"]))
    
    if(rownames(DivergenceList[["MACD"]])[1]=="MACD 上涨能量背驰"){
      DivergenceList[["BOLL信号"]]<-matrix(c(1,BOLL[which(BOLL$Date==as.character(Period$Out2)),]$PctB), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }else{
      DivergenceList[["BOLL信号"]]<-matrix(c(-1,BOLL[which(BOLL$Date==as.character(Period$Out2)),]$PctB), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }
    
    #DivergenceList[["VMACD"]]<-MACDCalculator(DataToBeTested, "VMACD", Period)         #this gives the Volume MACD
    
    DivergenceList[["ThreeLineDiv"]]<-MACDensemble[["ThreeLineDivMatrix"]]   #this gives the MACD
    DivergenceList[["Other"]]<-MACDensemble[["DivergenceMatrix"]][["Other"]]
  }else{print("There is no 中枢 to compare.")}
  return(DivergenceList)
}

CoDivergence<-function(DataToBeTested, Period=NULL, BarOverride=FALSE){
  CoDivergenceList<-list()
  CoDivergenceDivergenceMatrix<-as.data.frame(matrix(0, nrow = length(DataToBeTested)+1, ncol=5), stringsAsFactors=F)
  colnames(CoDivergenceDivergenceMatrix) <-c("中枢方向","MACD背驰强度","MFI背驰强度","形态背驰强度","BOLL信号")
  for (i in 1:length(DataToBeTested)) {
    CoDivergenceList[i]<-list(MACDPower(DataToBeTested=DataToBeTested[[i]], Period=Period[[i]], BarOverride[i]))
    rownames(CoDivergenceDivergenceMatrix)[i]<-names(DataToBeTested)[i]
    
    if(rownames(CoDivergenceList[[i]]$MACD)[1]=="MACD 上涨能量背驰"){
      CoDivergenceDivergenceMatrix[i,1]<-1}else{CoDivergenceDivergenceMatrix[i,1]<- -1
      }
    
    for (j in 2:4) {
      if(max(CoDivergenceList[[i]][[j]][,"背驰"])==1){
        CoDivergenceDivergenceMatrix[i,j]<-max(CoDivergenceList[[i]][[j]][,"强度"])
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