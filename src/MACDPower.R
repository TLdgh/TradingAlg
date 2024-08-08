MACDCalculator<-function(Pricedata, Data_macd, MACDType, Period, SBPStr, ScheduleAlert=FALSE){
  ####################################################################################################
  ####----Get the MACD area and reduction factor for the planet in order to consider divergence----###
  ####################################################################################################
  #calculate MACD of area 1 and 2
  A1_interval <- subset(Data_macd,Date>=Period$In1 & Date<=Period$In2)
  A2_interval <- subset(Data_macd,Date>=Period$Out1 & Date<=Period$Out2)
  A3_interval <- subset(Data_macd,Date>=Period$In2 & Date<=Period$Out1)
  Total_interval<-subset(Data_macd,Date>=Period$In1 & Date<=Period$Out2)
  
  In1Price<-SBPStr$StarData[which(SBPStr$StarData$Date==Period$In1),]$Price
  In2Price<-SBPStr$StarData[which(SBPStr$StarData$Date==Period$In2),]$Price
  Out1Price<-SBPStr$StarData[which(SBPStr$StarData$Date==Period$Out1),]$Price
  Out2Price<-SBPStr$StarData[which(SBPStr$StarData$Date==Period$Out2),]$Price
  
  if(In1Price<In2Price){ #price is going up
    Direction<-1
    #上涨能量背驰
    area_pos <- Total_interval[,c("MACD","Date")]%>%filter(., MACD>0)
    maxat=last(which(area_pos$MACD==max(area_pos$MACD)))
    Strength_pos<-ifelse(maxat!=nrow(area_pos), (nrow(area_pos)-maxat+1)/nrow(area_pos), 0)

    #上涨均线面积背驰
    EMALine_Pos<-Total_interval[,c("DEA","Date")]%>%filter(., DEA>0)
    maxat=which(EMALine_Pos$DEA==max(EMALine_Pos$DEA))
    EMALineStrength_Pos<-ifelse(maxat!=nrow(EMALine_Pos), (nrow(EMALine_Pos)-maxat+1)/nrow(EMALine_Pos), 0)
    
  }else{
    Direction<- -1
    #下跌能量背驰
    area_neg <- Total_interval[,c("MACD","Date")]%>%filter(., MACD<=0)
    minat=last(which(area_neg$MACD==min(area_neg$MACD)))
    Strength_neg<-ifelse(minat!=nrow(area_neg), (nrow(area_neg)-minat+1)/nrow(area_neg), 0)
    
    #下跌均线面积背驰
    EMALine_Neg<-Total_interval[,c("DEA","Date")]%>%filter(., DEA<=0)
    minat=which(EMALine_Neg$DEA==min(EMALine_Neg$DEA))
    EMALineStrength_Neg<-ifelse(minat!=nrow(EMALine_Neg), (nrow(EMALine_Neg)-minat+1)/nrow(EMALine_Neg), 0)
  }
  
  
  
  ####################################################################################################
  ####----------------------------Calculate the OpenPositionSignal---------------------------------###
  ####################################################################################################
  #This is 分型强度
  
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
  SignalStickReturn<-ROC(Pricedata[which(Pricedata$Date==Period$Out1):(which(Pricedata$Date==Period$Out2)+StopCount*CountIndex),]$Close,type="discrete")
  if(Direction==1){
    SignalStickRank<-which(sort(unique(SignalStickReturn), decreasing = FALSE)==tail(SignalStickReturn,1) )
  }else{
    SignalStickRank<-which(sort(unique(SignalStickReturn), decreasing = TRUE)==tail(SignalStickReturn,1) )
  }
  SignalStickRank<-ifelse(SignalStickRank<=3, 1, 0) #check if the stick is within the biggest three.
  
  
  
  ####################################################################################################
  ####----------------------------Calculate the length divergence----------------------------------###
  ####################################################################################################
  A_TimeWeight<-nrow(A2_interval)/nrow(A1_interval)

  Bar1length<-abs(In1Price-In2Price)    
  Bar2length<-abs(Out1Price-Out2Price)
  A_PriceWeight<-Bar2length/Bar1length
  
  StrcturalDiv<-mean(c(max(0, 1-A_TimeWeight), max(0, 1-A_PriceWeight)))
  StructuralDivMatrix<-matrix(c(StrcturalDiv>0,StrcturalDiv), nrow=1, ncol=2, dimnames = list("形态背驰", c("背驰","强度")))
  
  
  
  ####################################################################################################
  ####------------------------------------Check schedule alert-------------------------------------###
  ####################################################################################################
  #This function detects if a reverse has an macd bar surpassing the previous macd bars in a trend
  
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
  MacdMatrix<-matrix(0, nrow = 2, ncol = 2)
  colnames(MacdMatrix) <-c("背驰", "强度")
  if(MACDType=="MACD"){
    if(Direction==1){
      rownames(MacdMatrix)<-c("MACD 上涨能量背驰", "MACD 均线面积背驰")      
      MacdMatrix["MACD 上涨能量背驰","强度"]<-Strength_pos 
      MacdMatrix["MACD 上涨能量背驰","背驰"]<-(Strength_pos>0.33)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Pos
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALineStrength_Pos>0.33)
    }else{
      rownames(MacdMatrix)<-c("MACD 下跌能量背驰", "MACD 均线面积背驰")      
      MacdMatrix["MACD 下跌能量背驰","强度"]<-Strength_neg
      MacdMatrix["MACD 下跌能量背驰","背驰"]<-(Strength_neg>0.33)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Neg
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALineStrength_Neg>0.33)
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


MFICalculator<-function(Pricedata, Data_mfi, Data_MoneyFlow, StarData, Period){
  Total_interval<-subset(Data_mfi,Date>=Period$In1 & Date<=Period$Out2)
  Total_interval_MoneyFlow<-subset(Data_MoneyFlow,Date>=Period$In2 & Date<=Period$Out2)
  MFRatio<-Total_interval_MoneyFlow%>%summarise(Pos=sum(MoneyFlow[MoneyFlow>0]), Neg=sum(MoneyFlow[MoneyFlow<=0]))
  
  In1Price<-StarData[which(StarData$Date==Period$In1),]$Price
  In2Price<-StarData[which(StarData$Date==Period$In2),]$Price

  if(In1Price<In2Price){ #price is going up
    Direction<-1
    
    #上涨能量背驰
    area_pos <- Total_interval[,c("MFI","Date")]%>%filter(., MFI>0)
    maxat=last(which(area_pos$MFI==max(area_pos$MFI)))
    Strength_pos<-ifelse(maxat!=nrow(area_pos), (nrow(area_pos)-maxat+1)/nrow(area_pos), 0)

    #量比
    MFRatio<-MFRatio%>%summarise(Ratio=abs(Neg)/Pos)%>%as.numeric()
  }else{
    Direction<- -1
    
    #下跌能量背驰
    area_neg <- Total_interval[,c("MFI","Date")]%>%filter(., MFI<=0)
    minat=last(which(area_neg$MFI==min(area_neg$MFI)))
    Strength_neg<-ifelse(minat!=nrow(area_neg), (nrow(area_neg)-minat+1)/nrow(area_neg), 0)
    
    #量比
    MFRatio<-MFRatio%>%summarise(Ratio=Pos/abs(Neg))%>%as.numeric()
  }

  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 3)
  colnames(DivergenceMatrix) <-c("背驰", "强度", "量比")
  
  if(Direction==1){
    rownames(DivergenceMatrix)<-c("MFI 上涨背驰")      
    DivergenceMatrix["MFI 上涨背驰","强度"]<-(Strength_pos)
    DivergenceMatrix["MFI 上涨背驰","量比"]<-MFRatio
    DivergenceMatrix["MFI 上涨背驰","背驰"]<-(Strength_pos>0.33)
  }else{
    rownames(DivergenceMatrix)<-c("MFI 下跌背驰")      
    DivergenceMatrix["MFI 下跌背驰","强度"]<-(Strength_neg)
    DivergenceMatrix["MFI 下跌背驰","量比"]<-MFRatio
    DivergenceMatrix["MFI 下跌背驰","背驰"]<-(Strength_neg>0.33)
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
  Data_macd<-subset(Data_macd,Date>=Period$Out1)%>%
      mutate(D=c(0,diff(MACD)), Category=paste0(
        ifelse(MACD > 0, "positive_", "negative_"),
        consecutive_id(MACD > 0)),
        Lag=stats::lag(MACD,1)
      )%>%na.omit()%>%mutate(Pct=D/abs(Lag), DMean=(DIFF+DEA)/2)%>%
      mutate(x=1:nrow(.))
    
    res<-Data_macd%>%filter(Date>=Period$Out2 & Pct>0.5)
    Div<-(which(Data_macd$DMean==min(Data_macd$DMean))<nrow(Data_macd))
    numChunk<-Data_macd%>%filter(MACD<0)%>%group_by(Category)%>%summarise(PctMean=mean(Pct))%>%nrow()

  if(as.numeric(rownames(last(Data_macd))) <= as.numeric(rownames(Data_macd[which(Data_macd$Date==Period$Out2),]))+4){#check at most 4 bars after Out2
    if(Direction==-1 & Pricedata[nrow(Pricedata), "Low"]>Pricedata[which(Pricedata$Date==Period$Out2),"Low"]){
      Div<-(which(Data_macd$DMean==min(Data_macd$DMean))<nrow(Data_macd))
      numChunk<-Data_macd%>%filter(MACD<0)%>%group_by(Category)%>%summarise(PctMean=mean(Pct))%>%nrow()
      
      if(nrow(res)!=0 & numChunk==1){DivergenceMatrix[1,"macd"]<-1}
      else if(nrow(res)!=0 & Div==TRUE & numChunk>1){DivergenceMatrix[1,"macd"]<-2}
      else{DivergenceMatrix[1,"macd"]<-0}
    }
    else if(Direction==1 & Pricedata[nrow(Pricedata), "High"]<Pricedata[which(Pricedata$Date==Period$Out2),"High"]){
      Div<-(which(Data_macd$DMean==max(Data_macd$DMean))<nrow(Data_macd))
      numChunk<-Data_macd%>%filter(MACD>0)%>%group_by(Category)%>%summarise(PctMean=mean(Pct))%>%nrow()
      
      if(nrow(res)!=0 & numChunk==1){DivergenceMatrix[1,"macd"]<-1}
      else if(nrow(res)!=0 & Div==TRUE & numChunk>1){DivergenceMatrix[1,"macd"]<-2}
      else{DivergenceMatrix[1,"macd"]<-0}
    }
    else{res<-0}
  }
  else{DivergenceMatrix[1,"macd"]<-0}
  
  return(DivergenceMatrix)
}

MACDPower<-function(DataToBeTested, Period=NULL, BarOverride=FALSE, SBPStr=NULL, Data_macd=NULL, Data_mfi=NULL, Data_MoneyFlow=NULL, Data_boll=NULL, Data_ema=NULL, ScheduleAlert=FALSE){
  Pdate<-tail(DataToBeTested$Date,1)
  if(is.null(Data_macd)==TRUE){Data_macd<-PricedataMACD(DataToBeTested)}else{Data_macd<-Data_macd%>%filter(Date<=Pdate)}
  if(is.null(Data_mfi)==TRUE){Data_mfi<-PricedataMFI(DataToBeTested)}else{Data_mfi<-Data_mfi%>%filter(Date<=Pdate)}
  if(is.null(Data_MoneyFlow)==TRUE){Data_MoneyFlow<-PricedataMoneyFlow(DataToBeTested)}else{Data_MoneyFlow<-Data_MoneyFlow%>%filter(Date<=Pdate)}
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
    DivergenceList[["MFI"]]<-MFICalculator(DataToBeTested, Data_mfi=Data_mfi, Data_MoneyFlow=Data_MoneyFlow, StarData, Period)         #this gives the MFI
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