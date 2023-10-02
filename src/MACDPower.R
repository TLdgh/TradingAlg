MACDCalculator<-function(Pricedata, Data_macd, MACDType, Period, SBPStr, ScheduleAlert=FALSE){
  ####################################################################################################
  ####----Get the MACD area and reduction factor for the planet in order to consider divergence----###
  ####################################################################################################
  #calculate MACD of area 1 and 2
  A1_interval <- subset(Data_macd,Date>=Period$In1 & Date<=Period$In2)
  A2_interval <- subset(Data_macd,Date>=Period$Out1 & Date<=Period$Out2)
  A3_interval <- subset(Data_macd,Date>=Period$In2 & Date<=Period$Out1)
  PosNegMACDratio<-subset(Data_macd,Date>=Period$In1 & Date<=Period$Out2)%>%select(MACD)
  PosNegMACDratio<-sum(abs(PosNegMACDratio%>%filter(MACD>=0)))/sum(abs(PosNegMACDratio%>%filter(MACD<0)))
  
  #the range of time
  A1_length<-nrow(A1_interval)
  A2_length<-nrow(A2_interval)
  A_TimeWeight<-A2_length/A1_length
  
  #the range of price
  Bar1length<-abs(SBPStr$StarData[which(SBPStr$StarData$Date==Period$In1),]$Price-SBPStr$StarData[which(SBPStr$StarData$Date==Period$In2),]$Price)    
  Bar2length<-abs(SBPStr$StarData[which(SBPStr$StarData$Date==Period$Out1),]$Price-SBPStr$StarData[which(SBPStr$StarData$Date==Period$Out2),]$Price)
  A_PriceWeight<-Bar2length/Bar1length
  
  MeanWeight<-mean(A_TimeWeight,A_PriceWeight)
  
  if(SBPStr$StarData[which(SBPStr$StarData$Date==Period$In1),]$Price<SBPStr$StarData[which(SBPStr$StarData$Date==Period$In2),]$Price){ #price is going up
    Direction<-1
    area_pos1 <- A1_interval[which(A1_interval[,3]>0),3]
    area_pos2 <- A2_interval[which(A2_interval[,3]>0),3]
    area_pos1<-ifelse(length(area_pos1)>0, mean(area_pos1), 0)
    area_pos2<-ifelse(length(area_pos2)>0, mean(area_pos2)*MeanWeight, 0)
    Strength_pos<-ifelse(area_pos1!=0 & 1/PosNegMACDratio>=0.8,max(0,1-area_pos2/area_pos1), 0 )
    
    EMALine_Pos_1<-A1_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    EMALine_Pos_2<-A2_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    EMALine_Pos_3<-A3_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.>0))%>%select(DIFF,DEA)
    
    if(nrow(EMALine_Pos_1)>0){MeanPos1<-mean(rowMeans(EMALine_Pos_1)); EMALine_Pos_1<-sum(rowMeans(EMALine_Pos_1))}else{MeanPos1<-0; EMALine_Pos_1<-0}
    if(nrow(EMALine_Pos_2)>0){MeanPos2<-mean(rowMeans(EMALine_Pos_2)); EMALine_Pos_2<-sum(rowMeans(EMALine_Pos_2))}else{MeanPos2<-0; EMALine_Pos_2<-0}
    if(nrow(EMALine_Pos_3)>0){MeanPos3<-mean(rowMeans(EMALine_Pos_3))}else{MeanPos3<-0}
    
    Pos_ratio<-ifelse(MeanPos2==min(MeanPos1,MeanPos2,MeanPos3), 1, 1-MeanPos2/max(MeanPos1,MeanPos2,MeanPos3))
    EMALineStrength_Pos<-ifelse(EMALine_Pos_1!=0, max(0,1-EMALine_Pos_2/EMALine_Pos_1), 0)
  }else{
    Direction<- -1
    area_neg1 <- A1_interval[which(A1_interval[,3]<=0),3]
    area_neg2 <- A2_interval[which(A2_interval[,3]<=0),3]
    area_neg1<-ifelse(length(area_neg1)>0, mean(area_neg1), 0)
    area_neg2<-ifelse(length(area_neg2)>0, mean(area_neg2)*MeanWeight, 0)
    Strength_neg<-ifelse(area_neg1!=0 & PosNegMACDratio>=0.8,max(0,1-area_neg2/area_neg1), 0 )
    
    EMALine_Neg_1<-A1_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    EMALine_Neg_2<-A2_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    EMALine_Neg_3<-A3_interval[,c("DIFF","DEA","Date")]%>%filter_at(vars(DIFF,DEA), all_vars(.<=0))%>%select(DIFF,DEA)
    
    if(nrow(EMALine_Neg_1)>0){MeanNeg1<-abs(mean(rowMeans(EMALine_Neg_1))); EMALine_Neg_1<-abs(sum(rowMeans(EMALine_Neg_1)))}else{MeanNeg1<-0; EMALine_Neg_1<-0}
    if(nrow(EMALine_Neg_2)>0){MeanNeg2<-abs(mean(rowMeans(EMALine_Neg_2))); EMALine_Neg_2<-abs(sum(rowMeans(EMALine_Neg_2)))}else{MeanNeg2<-0; EMALine_Neg_2<-0}
    if(nrow(EMALine_Neg_3)>0){MeanNeg3<-abs(mean(rowMeans(EMALine_Neg_3)))}else{MeanNeg3<-0}
    
    Neg_ratio<-ifelse(MeanNeg2==min(MeanNeg1,MeanNeg2,MeanNeg3), 1, 1-MeanNeg2/max(MeanNeg1,MeanNeg2,MeanNeg3))
    EMALineStrength_Neg<-ifelse(EMALine_Neg_1!=0, max(0,1-EMALine_Neg_2/EMALine_Neg_1), 0)
  }
  
  
  
  ####################################################################################################
  ####----------------------------Calculate the OpenPositionSignal---------------------------------###
  ####################################################################################################
  StarBeginDate<-tail(A2_interval,2)$Date[1]
  StarBeginHigh<-Pricedata[which(Pricedata$Date==StarBeginDate),"High"]
  StarBeginLow<-Pricedata[which(Pricedata$Date==StarBeginDate),"Low"]
  
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
  
  StarMACDArea<-mean(tail(A2_interval,1+CountIndex)$MACD) #from the star bottom and count backwards of CountIndex number of candlesticks
  
  OpenPositionSignal<-function(){
    if(StopCount==1){
      SignalMacd<-Data_macd[which(Data_macd$Date==OpenPositionSignalDate),"MACD"]
      
      if(Direction==1){
        if(SignalMacd*StarMACDArea>0){x<- -(SignalMacd-StarMACDArea)/max(abs(SignalMacd),abs(StarMACDArea))}
        else{x<- -sign(SignalMacd-StarMACDArea)*abs(SignalMacd)/min(abs(SignalMacd),abs(StarMACDArea))}
      }else{
        if(SignalMacd*StarMACDArea>0){x<-(SignalMacd-StarMACDArea)/max(abs(SignalMacd),abs(StarMACDArea)) }
        else{x<-sign(SignalMacd-StarMACDArea)*abs(SignalMacd)/min(abs(SignalMacd),abs(StarMACDArea)) }
      }
    }else{x<-0}
    x<-ifelse(x>0, min(1,x), max(0,x))
    return(x)
  }
  
  
  
  ####################################################################################################
  ####------------------------Calculate the rank of the last three returns-------------------------###
  ####################################################################################################
  SignalStickReturn<-ROC(Pricedata[which(Pricedata$Date==Period$In1):(which(Pricedata$Date==Period$Out2)+StopCount*CountIndex),]$Close,type="discrete")
  if(Direction==1){
    SignalStickRank<-which(sort(unique(SignalStickReturn), decreasing = FALSE)==tail(SignalStickReturn,1) )
  }else{
    SignalStickRank<-which(sort(unique(SignalStickReturn), decreasing = TRUE)==tail(SignalStickReturn,1) )
  }
  SignalStickRank<-ifelse(SignalStickRank<=3, 1, 0) #check if the stick is within the biggest three.
  
  
  
  ####################################################################################################
  ####----------------------------Calculate the length divergence----------------------------------###
  ####################################################################################################
  StrcturalDiv<-mean(c(max(0, 1-A_TimeWeight), max(0, 1-A_PriceWeight)))
  StructuralDivMatrix<-matrix(c(StrcturalDiv>0,StrcturalDiv), nrow=1, ncol=2, dimnames = list("形态背驰", c("背驰","强度")))
  
  
  
  ####################################################################################################
  ####------------------------------------Check schedule alert-------------------------------------###
  ####################################################################################################
  if (ScheduleAlert==TRUE){
    NumBi<-ifelse(prod(tail(SBPStr$Bi,2)$SLOPE)>0, nrow(SBPStr$Bi)-1 ,nrow(SBPStr$Bi))
    OutIndex<-tail(SBPStr$BiPlanetStr,1)$OutIndex
    StopInd<-NumBi-OutIndex
    i<-1
    MACDalert<-0
    ComparePlanet<-TRUE
    
    while(i<=StopInd){
      if (Direction==1){
        if(i==1){
          AlertMACD<-Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex+1,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+1,"BiEndD"]),"MACD"]
          MinMACD<-0.94*min(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)}
        else if(i>=3){
          AlertMACD<-Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex+3,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+3,"BiEndD"]),"MACD"]
          
          if(SBPStr$Bi[OutIndex,"MAX"]<=SBPStr$Bi[OutIndex+2,"MAX"]){
            MinMACD<-min(Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+2,"BiEndD"]),"MACD"])
            OutIndex<-OutIndex+2
            ComparePlanet<-FALSE
          }else if(ComparePlanet==FALSE){MinMACD<-min(Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+2,"BiEndD"]),"MACD"])
          }else if(ComparePlanet==TRUE){MinMACD<-0.94*min(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)}
        }
        
        if(min(AlertMACD)<MinMACD){MACDalert<-1;break} #evaluate if there's an alert
        
      }else if(Direction== -1){
        if(i==1){
          AlertMACD<-Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex+1,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+1,"BiEndD"]),"MACD"]
          MaxMACD<-0.94*max(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)}
        else if(i>=3){
          AlertMACD<-Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex+3,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+3,"BiEndD"]),"MACD"]
          
          if(SBPStr$Bi[OutIndex,"MIN"]>=SBPStr$Bi[OutIndex+2,"MIN"]){
            MaxMACD<-max(Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+2,"BiEndD"]),"MACD"])
            OutIndex<-OutIndex+2
            ComparePlanet<-FALSE
          }else if(ComparePlanet==FALSE){MaxMACD<-max(Data_macd[which(Data_macd$Date>SBPStr$Bi[OutIndex,"BiStartD"] & Data_macd$Date<=SBPStr$Bi[OutIndex+2,"BiEndD"]),"MACD"])
          }else if(ComparePlanet==TRUE){MaxMACD<-0.94*max(c(A1_interval$MACD,A2_interval$MACD,A3_interval$MACD),na.rm = TRUE)}
        }
        
        if(max(AlertMACD)>MaxMACD){MACDalert<-1;break}
        
      }
      i<-i+2
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
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALineStrength_Pos>0)
      
      MacdMatrix["MACD 均线面积比例","强度"]<-Pos_ratio
      MacdMatrix["MACD 均线面积比例","背驰"]<-(round(Pos_ratio,2)>=0.6)
    }else{
      rownames(MacdMatrix)<-c("MACD 下跌能量背驰", "MACD 均线面积背驰", "MACD 均线面积比例")      
      MacdMatrix["MACD 下跌能量背驰","强度"]<-Strength_neg
      MacdMatrix["MACD 下跌能量背驰","背驰"]<-(Strength_neg>0)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Neg
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALineStrength_Neg>0)
      
      MacdMatrix["MACD 均线面积比例","强度"]<-Neg_ratio
      MacdMatrix["MACD 均线面积比例","背驰"]<-(round(Neg_ratio,2)>=0.6)
    }
  }#else{
  #if(Pricedata[which(Pricedata$Date==as.character(Period$In1)),]$High<Pricedata[which(Pricedata$Date==as.character(Period$Out2)),]$High){
  # rownames(DivergenceMatrix) <- c("Volume MACD 上涨能量背驰")
  #DivergenceMatrix["Volume MACD 上涨能量背驰","强度"]<-(1-abs(area_neg2)/abs(area_neg1))
  #DivergenceMatrix["Volume MACD 上涨能量背驰","背驰"]<-(DivergenceMatrix["Volume MACD 上涨能量背驰","强度"]>=0.6)
  #DivergenceMatrix["Volume MACD 上涨能量背驰","启动K线排名"]<-0
  #}else{
  # rownames(DivergenceMatrix) <- c("Volume MACD 下跌能量背驰")
  #DivergenceMatrix["Volume MACD 下跌能量背驰","强度"]<-(1-area_pos2/area_pos1)
  #DivergenceMatrix["Volume MACD 下跌能量背驰","背驰"]<-(DivergenceMatrix["Volume MACD 下跌能量背驰","强度"]>=0.6)
  #DivergenceMatrix["Volume MACD 下跌能量背驰","启动K线排名"]<-0
  #}
  #}
  #MacdMatrix2<-matrix(c(A_VolRatio, MaxWeight, A_Prob_R, A_ReductionFactor),nrow = 1, byrow = TRUE)
  #colnames(MacdMatrix2)<-c("A_VolRatio", "MaxWeight", "A_Prob_R", "A_ReductionFactor")
  #MacdMatrix2<-rbind(MacdMatrix2,MacdMatrix2)
  #MacdMatrix<-cbind(MacdMatrix,MacdMatrix2)
  
  if (ScheduleAlert==TRUE){
    Other<-matrix(c(SignalStickRank,OpenPositionSignal(),MACDalert),byrow=TRUE,nrow=1)
    colnames(Other)<-c("启动K线排名", "分型强度", "MACD Alert")
  }else{
    Other<-matrix(c(SignalStickRank,OpenPositionSignal()),byrow=TRUE,nrow=1)
    colnames(Other)<-c("启动K线排名", "分型强度")
  }
  
  DivergenceMatrix<-list(MacdMatrix=MacdMatrix, Other=Other)
  
  return(list(DivergenceMatrix=DivergenceMatrix, StructuralDivMatrix=StructuralDivMatrix))
}


MFICalculator<-function(Pricedata, Data_mfi, StarData, Period){
  A1_interval <- subset(Data_mfi,Date>= Period$In1 & Date<=Period$In2)
  A2_interval <- subset(Data_mfi,Date>= Period$Out1 & Date<=Period$Out2)
  
  #the range of time
  A1_length<-nrow(A1_interval)
  A2_length<-nrow(A2_interval)
  A_TimeWeight<-A2_length/A1_length
  
  #the range of price
  Bar1length<-abs(StarData[which(StarData$Date==Period$In1),]$Price-StarData[which(StarData$Date==Period$In2),]$Price)  
  Bar2length<-abs(StarData[which(StarData$Date==Period$Out1),]$Price-StarData[which(StarData$Date==Period$Out2),]$Price)
  A_PriceWeight<-Bar2length/Bar1length
  
  MeanWeight<-mean(c(A_TimeWeight,A_PriceWeight))
  
  #the average volume
  Vol1<-mean(subset(Pricedata,Date>= Period$In1 & Date<=Period$In2)$Volume )
  Vol2<-mean(subset(Pricedata,Date>= Period$Out1 & Date<=Period$Out2)$Volume )
  
  if(StarData[which(StarData$Date==Period$In1),]$Price<StarData[which(StarData$Date==Period$In2),]$Price){ #price is going up
    Direction<-1
    
    area_pos1 <- A1_interval[which(A1_interval[,2]>0),2]
    area_pos2 <- A2_interval[which(A2_interval[,2]>0),2]
    area_pos1<-ifelse(length(area_pos1)>0, mean(area_pos1), 1)
    area_pos2<-ifelse(length(area_pos2)>0, mean(area_pos2)*MeanWeight, 1)
    Strength_pos<-ifelse(area_pos1!=0, max(0,1-area_pos2/area_pos1), 0)
  }else{
    Direction<- -1
    area_neg1 <- A1_interval[which(A1_interval[,2]<=0),2]
    area_neg2 <- A2_interval[which(A2_interval[,2]<=0),2]
    
    area_neg1<-ifelse(length(area_neg1)>0, mean(area_neg1), 1)
    area_neg2<-ifelse(length(area_neg2)>0, mean(area_neg2)*MeanWeight, 1)
    Strength_neg<-ifelse(area_neg1!=0, max(0,1-area_neg2/area_neg1), 0)
  }
  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 3)
  colnames(DivergenceMatrix) <-c("背驰", "强度", "量比")
  
  if(Direction==1){
    rownames(DivergenceMatrix)<-c("MFI 上涨背驰")      
    DivergenceMatrix["MFI 上涨背驰","强度"]<-(Strength_pos)
    DivergenceMatrix["MFI 上涨背驰","量比"]<-max(0,(Vol1-Vol2)/Vol1)  #decreasing volume when price rising
    DivergenceMatrix["MFI 上涨背驰","背驰"]<-(Strength_pos>0)
  }else{
    rownames(DivergenceMatrix)<-c("MFI 下跌背驰")      
    DivergenceMatrix["MFI 下跌背驰","强度"]<-(Strength_neg)
    DivergenceMatrix["MFI 下跌背驰","量比"]<-max(0,(Vol1-Vol2)/Vol1) #decreasing volume when price dropping
    DivergenceMatrix["MFI 下跌背驰","背驰"]<-(Strength_neg>0)
  }
  return(DivergenceMatrix)
}

EMACalculator<-function(Pricedata, Data_ema, Data_macd, Period){
  #initialize
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 2)
  colnames(DivergenceMatrix) <-c("Entanglement", "macd")
  Direction<-ifelse(Pricedata[which(Pricedata$Date==Period$In1),"Close"]<Pricedata[which(Pricedata$Date==Period$Out2),"Close"], 1, -1)
  
  #check EMA
  A1_interval <- map(Data_ema, function(x) subset(x, Date>=Period$In2 & Date<=Period$Out1))
  if((Direction==-1 & any(mapply(function(x, y){return(x>y)}, A1_interval[[1]]$EMA10, A1_interval[[2]]$EMA60))) |
     ((Direction==1 & any(mapply(function(x, y){return(x<=y)}, A1_interval[[1]]$EMA10, A1_interval[[2]]$EMA60))))
     ){
    DivergenceMatrix[1,"Entanglement"]<-1
  }else{
    DivergenceMatrix[1,"Entanglement"]<-0}
  
  #check if any macd bar changes more than 50% within a period
  Data_macd<-subset(Data_macd,Date>=Period$Out1)
  if(as.numeric(rownames(last(Data_macd))) <= as.numeric(rownames(Data_macd[which(Data_macd$Date==Period$Out2),]))+4){#check at most 3 bars after Out2
    res<-round(diff(Data_macd$MACD)/abs(Data_macd$MACD[-nrow(Data_macd)]),2)
    if(Direction==-1){res<-res}else{res<- -res}
    if(any(res[res>0]>0.5)){DivergenceMatrix[1,"macd"]<-1}else{DivergenceMatrix[1,"macd"]<-0}
  }else{
    DivergenceMatrix[1,"macd"]<-0
  }
  return(DivergenceMatrix)
}

MACDPower<-function(DataToBeTested, Period=NULL, BarOverride=FALSE, SBPStr=NULL, Data_macd=NULL, Data_mfi=NULL, Data_boll=NULL, Data_ema=NULL, ScheduleAlert=FALSE){
  Pdate<-tail(DataToBeTested$Date,1)
  if(is.null(Data_macd)==TRUE){Data_macd<-PricedataMACD(DataToBeTested)}else{Data_macd<-Data_macd%>%filter(Date<=Pdate)}
  if(is.null(Data_mfi)==TRUE){Data_mfi<-PricedataMFI(DataToBeTested)}else{Data_mfi<-Data_mfi%>%filter(Date<=Pdate)}
  if(is.null(Data_boll)==TRUE){Data_boll<-PricedataBOLL(DataToBeTested)}else{Data_boll<-Data_boll%>%filter(Date<=Pdate)}
  if(is.null(Data_ema)==TRUE){Data_ema<-list(EMA10=FuncEMA10(DataToBeTested), EMA60=FuncEMA60(DataToBeTested))}else{Data_ema<-Data_ema%>%filter(Date<=Pdate)}
  if(is.null(SBPStr)==TRUE){
    SBPStr<-ChanLunStr(DataToBeTested)
    StarData<-SBPStr$StarData
    Bi<-SBPStr$Bi
    BiPlanetStr<-SBPStr$BiPlanetStr
    Finalplanet<-tail(BiPlanetStr,1)
  }else{
    StarData<-SBPStr$StarData%>%filter(Date<=Pdate)
    Bi<-SBPStr$Bi%>%filter(BiEndD<=Pdate)
    BiPlanetStr<-SBPStr$BiPlanetStr%>%filter(BiEndD<=Pdate)
    Finalplanet<-tail(BiPlanetStr,1)
  }
  
  OutIndex<-Finalplanet$OutIndex
  if(BarOverride==FALSE){
    In1<-Finalplanet$BiStartD #this gives the start date of the incoming bi of the final planet
    In2<-Finalplanet$PlanetStartD #this gives the end date of the incoming bi of the final planet
    Out1<-Finalplanet$PlanetEndD #this gives the start date of the leaving bi of the final planet
    
    if(Finalplanet$SLOPE==1 
       & OutIndex<=(nrow(Bi)-2) 
       & Bi[OutIndex+2,"MIN"]>=Finalplanet$PlanetHigh 
       & Bi[OutIndex+2,"MAX"]>=Bi[OutIndex,"MAX"]){
      Out2 <- Bi[OutIndex+2,"BiEndD"]
    }else if(Finalplanet$SLOPE== -1 
             & OutIndex<=(nrow(Bi)-2) 
             & Bi[OutIndex+2,"MAX"]<=Finalplanet$PlanetLow 
             & Bi[OutIndex+2,"MIN"]<=Bi[OutIndex,"MIN"]){
      Out2 <- Bi[OutIndex+2,"BiEndD"]
    }else{Out2<-Finalplanet$BiEndD}
    
    Period <- data.frame(In1,In2,Out1,Out2) #this gives the end date of the leaving bi of the final planet
  }
  
  DivergenceList<-list()
  if (nrow(Finalplanet)!=0 | BarOverride==TRUE){
    MACDensemble<-MACDCalculator(Pricedata=DataToBeTested,Data_macd=Data_macd,MACDType="MACD",Period,SBPStr,ScheduleAlert)
    DivergenceList[["Period"]]<-Period
    DivergenceList[["MACD"]]<-MACDensemble[["DivergenceMatrix"]][["MacdMatrix"]]   #this gives the MACD
    DivergenceList[["MFI"]]<-MFICalculator(DataToBeTested, Data_mfi=Data_mfi, StarData, Period)         #this gives the MFI
    DivergenceList[["EMA"]]<-EMACalculator(DataToBeTested, Data_ema=Data_ema, Data_macd=Data_macd, Period)  
    
    DivergenceList[["形态背驰"]]<-MACDensemble[["StructuralDivMatrix"]]
    
    if(rownames(DivergenceList[["MACD"]])[1]=="MACD 上涨能量背驰"){
      boll<-Data_boll[which(Data_boll$Date==as.character(Period$Out2)),]$PctB
      DivergenceList[["BOLL信号"]]<-matrix(c(1,ifelse(boll>0.85, min(1,boll), max(0,boll)) ), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }else{
      boll<-abs(1-Data_boll[which(Data_boll$Date==as.character(Period$Out2)),]$PctB)
      DivergenceList[["BOLL信号"]]<-matrix(c(-1,ifelse(boll>0.85, min(1,boll), max(0,boll)) ), nrow=1, byrow=TRUE)
      colnames(DivergenceList[["BOLL信号"]]) <-c("方向", "强度")
    }
    
    DivergenceList[["Other"]]<-MACDensemble[["DivergenceMatrix"]][["Other"]]
    
    DivergenceList[["Class"]]<-paste(c(as.numeric(DivergenceList[["MACD"]][,1]),
                                       as.numeric(DivergenceList[["MFI"]][,1]),
                                       as.numeric(DivergenceList[["形态背驰"]][,1]),
                                       as.numeric(DivergenceList[["BOLL信号"]][,2]>0),
                                       as.numeric(DivergenceList[["Other"]][,1]!=0),
                                       as.numeric(DivergenceList[["Other"]][,2]>0)), collapse = "")
  }else{print("There is no 中枢 to compare.")}
  return(DivergenceList)
}

CoDivergence<-function(DataToBeTested, Period=NULL, BarOverride=FALSE, Data_macd=NULL, Data_mfi=NULL,Data_boll=NULL){
  CoDivergenceList<-list()
  CoDivergenceDivergenceMatrix<-as.data.frame(matrix(0, nrow = length(DataToBeTested)+1, ncol=5), stringsAsFactors=F)
  colnames(CoDivergenceDivergenceMatrix) <-c("中枢方向","MACD背驰强度","MFI背驰强度","形态背驰强度","BOLL信号")
  for (i in 1:length(DataToBeTested)) {
    CoDivergenceList[i]<-list(MACDPower(DataToBeTested=DataToBeTested[[i]], Data_macd=Data_macd,Data_mfi=Data_mfi,Data_boll=Data_boll,Period=Period[[i]], BarOverride[i]))
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