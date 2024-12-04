split_interval<-function(interval){
  res=interval%>%mutate(group=cumsum(MACD * lag(MACD, default = first(MACD))<=0))%>%
    group_by(group)%>%group_split()
  return(res)
}

checkdiv<-function(intervals, dir){
  if(dir==-1){
    
    A1df=split_interval(intervals$l1)%>%
      map(~filter(.x, DIFF<=0 & DEA<=0))%>%
      keep(~nrow(.x)>=1 && all(.x$MACD<=0))
    
    A2df=split_interval(intervals$l2)%>%
      map(~filter(.x, DIFF<=0 & DEA<=0))%>%
      keep(~nrow(.x)>=5 && all(.x$MACD<=0))
    
    
    if(length(A2df)<2){df=c(A1df, A2df)}else{df=A2df}
    if(length(df)<2){print("not enough MACD histograms to be considered. Inconclusive!");return(0)}
    else{
      minDEA=df%>%map(~min(.x$DEA))%>%unlist()
      #print(minDEA)
      last_df=df[[length(df)]]$MACD
      second_last_df=df[[length(df)-1]]$MACD
      compareMACD=length(which(last_df>min(second_last_df)))/length(last_df)
      #print(compareMACD)
      if(length(compareMACD)!=0){
        return(mean(c(last(minDEA)>minDEA[length(minDEA)-1], compareMACD>=0.7)))
      }else{return(0)}
    }
    
    
    
  }
  else{
    
    A1df=split_interval(intervals$l1)%>%
      map(~filter(.x, DIFF>0 & DEA>0))%>%
      keep(~nrow(.x)>5 && all(.x$MACD>0))
    
    A2df=split_interval(intervals$l2)%>%
      map(~filter(.x, DIFF>0 & DEA>0))%>%
      keep(~nrow(.x)>5 && all(.x$MACD>0))
    
    
    if(length(A2df)<2){df=c(A1df, A2df)}else{df=A2df}
    if(length(A2df)==0 | length(df)<2){print("not enough MACD histograms to be considered. Inconclusive!");return(0)}
    
    maxDEA=df%>%map(~max(.x$DEA))%>%unlist()
    last_df=df[[length(df)]]$MACD
    second_last_df=df[[length(df)-1]]$MACD
    compareMACD=length(which(last_df<=max(second_last_df)))/length(last_df)
    if(length(compareMACD)!=0){
      return(mean(c(last(maxDEA)<maxDEA[length(maxDEA)-1], compareMACD>=0.7)))
    }else{return(0)}
    
  }
  
}

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
    Strength_pos<-checkdiv(intervals =list(l1=A1_interval, l2=A2_interval), dir = Direction)
    
    #上涨均线面积背驰
    EMALine_Pos<-Total_interval[,c("DEA","Date")]%>%filter(., DEA>0)
    maxat=last(which(EMALine_Pos$DEA==max(EMALine_Pos$DEA)))
    EMALineStrength_Pos<-ifelse(maxat!=nrow(EMALine_Pos), (nrow(EMALine_Pos)-maxat+1)/nrow(EMALine_Pos), 0)
    
  }else{
    Direction<- -1
    #下跌能量背驰
    Strength_neg<-checkdiv(intervals =list(l1=A1_interval, l2=A2_interval), dir = Direction)
    
    #下跌均线面积背驰
    EMALine_Neg<-Total_interval[,c("DEA","Date")]%>%filter(., DEA<=0)
    minat=last(which(EMALine_Neg$DEA==min(EMALine_Neg$DEA)))
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
      MacdMatrix["MACD 上涨能量背驰","背驰"]<-(Strength_pos>0)
      
      MacdMatrix["MACD 均线面积背驰","强度"]<-EMALineStrength_Pos
      MacdMatrix["MACD 均线面积背驰","背驰"]<-(EMALineStrength_Pos>0.33)
    }else{
      rownames(MacdMatrix)<-c("MACD 下跌能量背驰", "MACD 均线面积背驰")      
      MacdMatrix["MACD 下跌能量背驰","强度"]<-Strength_neg
      MacdMatrix["MACD 下跌能量背驰","背驰"]<-(Strength_neg>0)
      
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
  MoneyFlow1<-subset(Data_MoneyFlow,Date>=Period$In1 & Date<=Period$In2)
  MoneyFlow2<-subset(Data_MoneyFlow,Date>=Period$Out1 & Date<=Period$Out2)
  MFRatio <- list(MoneyFlow1, MoneyFlow2) %>%
    map(~ .x %>%summarise(Pos = sum(MoneyFlow_EMA[MoneyFlow_EMA > 0], na.rm = TRUE),
                          Neg = sum(MoneyFlow_EMA[MoneyFlow_EMA <= 0], na.rm = TRUE))
    )
  
  In1Price<-StarData[which(StarData$Date==Period$In1),]$Price
  In2Price<-StarData[which(StarData$Date==Period$In2),]$Price
  
  if(In1Price<In2Price){ #price is going up
    Direction<-1
    
    #上涨能量背驰
    area_pos <- Total_interval[,c("MFI","Date")]%>%filter(., MFI>0)
    maxat=last(which(area_pos$MFI==max(area_pos$MFI)))
    Strength_pos<-ifelse(maxat!=nrow(area_pos), (nrow(area_pos)-maxat+1)/nrow(area_pos), 0)
    
    #量比
    MFRatio<- 1-MFRatio[[2]]$Pos/MFRatio[[1]]$Pos
  }else{
    Direction<- -1
    
    #下跌能量背驰
    area_neg <- Total_interval[,c("MFI","Date")]%>%filter(., MFI<=0)
    minat=last(which(area_neg$MFI==min(area_neg$MFI)))
    Strength_neg<-ifelse(minat!=nrow(area_neg), (nrow(area_neg)-minat+1)/nrow(area_neg), 0)
    
    #量比
    MFRatio<-1-MFRatio[[2]]$Neg/MFRatio[[1]]$Neg
  }
  
  
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 3)
  colnames(DivergenceMatrix) <-c("背驰", "强度", "量比")
  
  if(Direction==1){
    rownames(DivergenceMatrix)<-c("MFI 上涨背驰")      
    DivergenceMatrix["MFI 上涨背驰","强度"]<-(Strength_pos)
    DivergenceMatrix["MFI 上涨背驰","量比"]<-MFRatio
    DivergenceMatrix["MFI 上涨背驰","背驰"]<-(Strength_pos>0.33 & MFRatio>0)
  }else{
    rownames(DivergenceMatrix)<-c("MFI 下跌背驰")      
    DivergenceMatrix["MFI 下跌背驰","强度"]<-(Strength_neg)
    DivergenceMatrix["MFI 下跌背驰","量比"]<-MFRatio
    DivergenceMatrix["MFI 下跌背驰","背驰"]<-(Strength_neg>0.33 & MFRatio>0)
  }
  return(DivergenceMatrix)
}

EMACalculator<-function(Pricedata, Data_ema, Period){
  #initialize
  DivergenceMatrix<-matrix(0, nrow = 1, ncol = 2)
  colnames(DivergenceMatrix) <-c("Entanglement", "macd")
  Direction<-ifelse(Pricedata[which(Pricedata$Date==Period$In1),"Close"]<Pricedata[which(Pricedata$Date==Period$Out2),"Close"], 1, -1)
  
  #check EMA
  A1_interval <- map(Data_ema, function(x) subset(x, Date>=Period$In2 & Date<=Period$Out1))
  if((Direction==-1 & any(mapply(function(x, y){return(x>y)}, A1_interval[[1]]$EMA5, A1_interval[[2]]$EMA20))) |
     ((Direction==1 & any(mapply(function(x, y){return(x<=y)}, A1_interval[[1]]$EMA5, A1_interval[[2]]$EMA20))))
  ){
    DivergenceMatrix[1,"Entanglement"]<-1
  }else{
    DivergenceMatrix[1,"Entanglement"]<-0}
  
  DivergenceMatrix[1,"macd"]<-0
  return(DivergenceMatrix)
}

MACDPower<-function(DataToBeTested, Title, Period=NULL, BarOverride=FALSE, SBPStr=NULL, Data_macd=NULL, Data_mfi=NULL, Data_MoneyFlow=NULL, Data_boll=NULL, Data_ema=NULL, ScheduleAlert=FALSE){
  Pdate<-tail(DataToBeTested$Date,1)
  if(is.null(Data_macd)==TRUE){Data_macd<-PricedataMACD(DataToBeTested)}else{Data_macd<-Data_macd%>%filter(Date<=Pdate)}
  if(is.null(Data_mfi)==TRUE){Data_mfi<-PricedataMFI(DataToBeTested)}else{Data_mfi<-Data_mfi%>%filter(Date<=Pdate)}
  if(is.null(Data_MoneyFlow)==TRUE){Data_MoneyFlow<-PricedataMoneyFlow(DataToBeTested)}else{Data_MoneyFlow<-Data_MoneyFlow%>%filter(Date<=Pdate)}
  if(is.null(Data_boll)==TRUE){Data_boll<-PricedataBOLL(DataToBeTested)}else{Data_boll<-Data_boll%>%filter(Date<=Pdate)}
  if(is.null(Data_ema)==TRUE){Data_ema<-list(EMA5=FuncEMA5(DataToBeTested), EMA20=FuncEMA20(DataToBeTested))}else{Data_ema<-Data_ema%>%filter(Date<=Pdate)}
  
  
  if(is.null(SBPStr)==TRUE){
    if(substr(Title, 1,2)=="NQ"){combfile_path=paste0(getwd(),"/CandleStickComb/NQ/",Title, "Comb.csv")}
    else if(str_sub(Title,-5)=="daily"){combfile_path=paste0(getwd(),"/CandleStickComb/US/",Title, "Comb.csv")}
    PricedataComb=read.csv(combfile_path)%>%arrange(Date)%>%subset(Date>=first(DataToBeTested$Date) & Date<=last(DataToBeTested$Date))
    SBPStr<-ChanLunStr(PricedataComb)
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
    DivergenceList[["EMA"]]<-EMACalculator(DataToBeTested, Data_ema=Data_ema, Period)  
    
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



getMins<-function(BreakoutStructure,d1,d2,d3,d4){
  previousLow <- lapply(BreakoutStructure[c("Price", "MACD","MF")], function(df) {
    subset(df, Date >= d1 & Date <= d2)
  })
  minRet=previousLow[['Price']]%>%transmute(Date, Value=log(Close/Open))%>%arrange(Value, Date)%>%slice_min(order_by = Value, n = 1, with_ties = FALSE)
  minMacd=previousLow[['MACD']]%>%select(c(Date, Value=MACD))%>%arrange(Value, Date)%>%slice_min(order_by = Value, n = 1, with_ties = FALSE)
  minMF=previousLow[['MF']]%>%select(c(Date, Value=MoneyFlow))%>%arrange(Value, Date)%>%slice_min(order_by = Value, n = 1, with_ties = FALSE)
  
  currentLow <- lapply(BreakoutStructure[c("Price", "MACD","MF")], function(df) {
    subset(df, Date >= d3 & Date <= d4)})
  currentRet=currentLow[['Price']]%>%transmute(Date, Value=log(Close/Open))%>%filter(Value<=minRet$Value)%>%first()
  currentMacd=currentLow[['MACD']]%>%select(c(Date, Value=MACD))%>%filter(Value<=minMacd$Value)%>%first()
  currentMF=currentLow[['MF']]%>%select(c(Date, Value=MoneyFlow))%>%filter(Value<=minMF$Value)%>%first()
  
  return(list(pricebreak=currentRet$Date, macdbreak=currentMacd$Date, mfbreak=currentMF$Date)) 
}

#This is the function to check the strategy at any customized time, or of the most recent breakout structure
LatestBreakout<-function(CombData, specifyDate=NULL){
  Data_macd<-PricedataMACD(CombData) #calculate the MACD
  Data_MF<-PricedataMoneyFlow(CombData)
  Data_MFI<-PricedataMFI(CombData)
  Data_EMA30<-FuncEMA30(CombData)
  Data_EMA60<-FuncEMA60(CombData)
  
  #Make sure to start from the date on which all data have no NA
  minimumdate=map(list(CombData, Data_macd, Data_MF,Data_MFI,Data_EMA60), ~.x$Date)%>%Reduce(intersect, .)%>%first()
  SBPStr<-ChanLunStr(CombData)
  Bi<-SBPStr$Bi
  BiPlanetStr<-SBPStr$BiPlanetStr
  Bi=filter(Bi,BiStartD>=minimumdate)
  
  i=ifelse(is.null(specifyDate), nrow(Bi)-3, which(Bi$BiStartD==specifyDate))
  
  while(i>=1){
    if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
      start_ind=Bi$BiStartD[i]
      
      BreakoutStructure=list(Bi=filter(Bi,BiStartD>=start_ind), 
                             Price=filter(CombData,Date>=start_ind),
                             MACD=filter(Data_macd,Date>=start_ind),
                             MF=filter(Data_MF,Date>=start_ind),
                             MFI=filter(Data_MFI,Date>=start_ind)
      )
      
      timebreakhigh=filter(BreakoutStructure$Price, Date>=BreakoutStructure$Bi[1+3,"BiStartD"])
      indexbreakhigh=which(timebreakhigh$High>=BreakoutStructure$Bi[1+2,"MAX"]) #find the indices on which prices break up.
      if(length(indexbreakhigh)!=0){      
        ordertime=timebreakhigh[indexbreakhigh, "Date"]%>%ymd_hms(tz="America/Toronto", quiet = TRUE)%>%{ ifelse(!is.na(.), hour(.), NA) } #get the hours, or all NA if daily.
      }else{ordertime=NA}
      if(!all(is.na(ordertime)) && any(ordertime>=6)){ind1=indexbreakhigh[min(which(ordertime>=6))]}else{ind1=first(indexbreakhigh)} #if not all NA and there's hour>=6
      
      #check MACD reversal
      macd_rev1=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      macd_rev1_bygroup=macd_rev1%>%split_interval()#group the macd into pos and neg values
      macd_rev2=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
      revindex1 <- which(sapply(macd_rev1_bygroup, function(df) any(df$MACD>0))) #get the index of the df whose MACD>0
      if(length(revindex1)!=0){lastMaxMacd=max(macd_rev1_bygroup[[last(revindex1)]]$MACD)}else{lastMaxMacd=0} #the max of the last positive MACD group
      
      mf_rev1=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      mf_rev2=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
      maxMF=list(mf_rev1,mf_rev2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = TRUE)[1:3]))
      maxMF_EMA=list(mf_rev1,mf_rev2)%>%map(~max(.x$MoneyFlow_EMA))
      
      if(length(revindex1)==0){rev=1}
      else if((length(revindex1)!=0 & max(macd_rev2$MACD) >= 0.97*lastMaxMacd) &
              (maxMF[[2]]>=maxMF[[1]])
      ){rev=1}else{rev=0}
      
      
      
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
      mfema1 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1,"BiEndD"])
      mfema2 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      
      minMF<-list(mf_div1,mf_div2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = FALSE)[1:3]))
      minMFI<-list(mfi_div1,mfi_div2)%>%map(~min(.x$MFI))
      #量比
      MFRatio <- list(mfema1, mfema2) %>%
        map(~ .x %>%summarise(Pos = sum(MoneyFlow_EMA[MoneyFlow_EMA > 0], na.rm = TRUE),
                              Neg = sum(MoneyFlow_EMA[MoneyFlow_EMA <= 0], na.rm = TRUE))
        )
      mfr<-1-MFRatio[[2]]$Neg/MFRatio[[1]]$Neg
      
      #MACDPower
      powerind=which(BiPlanetStr$BiEndD==BreakoutStructure$Bi[1+2,"BiEndD"])
      if(length(powerind)!=0){
        d1=CombData[max(1,which(CombData$Date==BiPlanetStr[powerind, 'BiStartD'])-500), "Date"]
        d2=BreakoutStructure$Bi[1+3,"BiEndD"]
        cat("Out2",BreakoutStructure$Bi[1+2,"BiEndD"],'\n')
        powerlist=MACDPower(filter(CombData,  Date>=d1 & Date<=d2),SBPStr = SBPStr)
        power_res=sum(as.numeric(strsplit(powerlist$Class, "")[[1]]))
        print(power_res)
      }else{power_res=NULL}
      
      if(falsebreakout==1){div=1}
      else if(
        (min(macd_div2$MACD) > min(macd_div1$MACD)) & 
        ((0.999*minMF[[2]]>minMF[[1]]) | (minMFI[[2]]>minMFI[[1]]) | (mfr>0))     
      ){div=1}else{div=0}
      
      
      
      res1 <- data.frame(
        Key = c(
          "MACD1", "MACD2", 
          "MF1", "MF2", 
          "Period Start", "Period End", 
          "Reversal", "False Breakout"
        ),
        Value = c(
          0.97*lastMaxMacd, max(macd_rev2$MACD), 
          maxMF[[1]], maxMF[[2]], 
          BreakoutStructure$Bi[1 + 1, "BiStartD"], 
          BreakoutStructure$Bi[1 + 3, "BiEndD"],
          as.logical(rev), falsebreakout
        ),
        stringsAsFactors = FALSE
      )
      
      res2 <- data.frame(
        Key = c(
          "MACD1", "MACD2", 
          "MF1", "MF2", 
          "MFI1", "MFI2", 
          "MFRatio1","MFRatio2",
          "Period Start", "Period End", 
          "Divergence", "Macdpower", "False Breakout"
        ),
        Value = c(
          min(macd_div1$MACD), min(macd_div2$MACD), 
          minMF[[1]], 0.999*minMF[[2]], 
          minMFI[[1]], minMFI[[2]], 
          MFRatio[[1]]$Neg,MFRatio[[2]]$Neg,
          BreakoutStructure$Bi[1, "BiStartD"], 
          BreakoutStructure$Bi[1 + 2, "BiEndD"],
          as.logical(div), ifelse(!is.null(power_res) && power_res > 5, TRUE, FALSE), falsebreakout
        ),
        stringsAsFactors = FALSE
      )
      
      # Print the data frames
      cat("\nReversal Information:\n")
      print(res1)
      cat("\nDivergence Information:\n")
      print(res2)
      
      
      
      # MACD 和 MFI 创新高，时间7点以后，买入
      if( ((rev==1 & div==1)|( !is.null(power_res) && power_res > 5)) & 
          ((!all(is.na(ordertime)) && any(ordertime>=6 & ordertime<=23) ) | all(is.na(ordertime)) ) ){ #if is na, it means it's daily/weekly time
        ExistPosition=TRUE
        
        #The target date is the latest date when both MACD and MF break high, and if this date is before the one when price breaks high, we have a better price and can enter earlier.
        target_date_macd=ifelse(length(revindex1)!=0, macd_rev2[first(which(macd_rev2$MACD>lastMaxMacd)),"Date"], macd_rev2[first(which(macd_rev2$MACD>0)),"Date"])
        target_date_mf=mf_rev2[which(mf_rev2$MoneyFlow>=max(mf_rev1$MoneyFlow)),"Date"]%>%first()
        target_date=max(target_date_macd,target_date_mf)
        ind2=which(timebreakhigh$Date==target_date)
        buyP=min(BreakoutStructure$Bi[1+2,"MAX"], timebreakhigh[ind2, "Close"])
        cat("Open position price:", buyP, "Open position time:", min(timebreakhigh[c(ind1, ind2),"Date"]), '\n')
      }else{
        ExistPosition=FALSE
        cat(
          "No position should be opened.",
          if (rev == 0) "---rev not satisfied.",
          if (div == 0) "---div not satisfied.",
          if ((!all(is.na(ordertime)) && (all(ordertime > 23) | all(ordertime < 6)) ) | all(is.na(ordertime))) "---ordertime not satisfied.",
          '\n'
        )
        break
      }
      
      
      
      j=2
      stoploss=BreakoutStructure$Bi[1+2,"MIN"] #止损在笔2的最低点
      profittaker=0
      
      while((1+2+j)<=nrow(BreakoutStructure$Bi) & ExistPosition){
        if(BreakoutStructure$Bi[1+3,"MAX"]>=BreakoutStructure$Bi[1+2+j,"MAX"]){ #如果笔4及以后的下降笔最高点小于笔3最高点，也就是在笔3区间内盘整
          # Consolidate clear positions
          ClearPosition=getMins(BreakoutStructure, 
                                d1=BreakoutStructure$Bi[1+2+j-2,"BiStartD"], 
                                d2=BreakoutStructure$Bi[1+2+j-2,"BiEndD"],
                                d3=BreakoutStructure$Bi[1+2+j,"BiStartD"], 
                                d4=BreakoutStructure$Bi[1+2+j,"BiEndD"])%>%keep(~ length(.x) > 0)

          #print(ClearPosition)
          if(length(ClearPosition)!=0){
            ClearPosition=ClearPosition%>%unlist()%>%sort()
            accP=CombData$Close[match(ClearPosition, CombData$Date)]
            accPind=accP<Data_EMA30$EMA30[match(ClearPosition, Data_EMA30$Date)]
            accPind=which(accPind==TRUE)
            # This checks if stoploss happens before accDecrease
            anylower=filter(CombData, Date>=BreakoutStructure$Bi[1+2+j,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2+j,"BiEndD"] & Low<=stoploss)%>%first()
            if(nrow(anylower)==1 & ( !is.null(accPind) && length(accPind)>0 && ClearPosition[accPind[1]]>anylower$Date)){accP=NULL;accPind=NULL}
          }else{
            accP=NULL
            accPind=NULL
          }
          
          if((is.null(accPind) | ( !is.null(accPind) && length(accPind)==0) ) &
             BreakoutStructure$Bi[1+2+j,"MIN"]<=stoploss){ 
            #任何时候下降笔破止损就卖出
            sellP=stoploss
            sellReason="stoploss"
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            sellRefDate=BreakoutStructure$Bi[1+2,"BiEndD"]
            cat("stoploss was triggered!", "sellRefDate:", sellRefDate, "value:", sellP, "\n")
            break}
          else if( !is.null(accPind) && length(accPind)>1){ 
            #加速下跌，保本。如果不破止损就提前走，否则止损
            sellP=max(stoploss, accP[accPind[1]])
            sellReason="acceDecrease"
            sellRefDate=ClearPosition[accPind[1]]%>%as.character()
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            cat(paste0("acceDecrease due to ", names(ClearPosition[accPind[1]]), "."), "Exit immediately!", "sellRefDate:", sellRefDate, "value:", sellP, "\n")
            break}
          else if((1+2+j)>=nrow(BreakoutStructure$Bi)){
            cat("No stoploss triggered.", "sellRefDate:", BreakoutStructure$Bi[1+2,"BiEndD"], "value:", stoploss, "\n")
            break}
          else{j=j+2} #以上都不满足则继续笔3区间震荡
        }
        else{ #如果突破笔3最高点，则止盈开始。止盈位是前低或者进场k线的最低点，取最大
          breakema60 <- map(list(data=BreakoutStructure$Price, ema60=Data_EMA60), ~ {
            filter(.x, Date >= BreakoutStructure$Bi$BiStartD[1+2+j] & Date <= BreakoutStructure$Bi$BiEndD[1+2+j])
          })
          breakema60=breakema60$data[which(breakema60$data$Close<breakema60$ema60$EMA60)%>%first(),]
          profitlevel=list(Date=c(BreakoutStructure$Bi$BiEndD[1+2], BreakoutStructure$Bi$BiEndD[1+2+j], BreakoutStructure$Bi$BiEndD[1+2+j-2], breakema60$Date), 
                           Value=c(BreakoutStructure$Bi$MIN[1+2], mean(c(BreakoutStructure$Bi$MIN[1+2],BreakoutStructure$Bi$MAX[1+2])), BreakoutStructure$Bi$MIN[1+2+j-2]*0.999, breakema60$Close))
          
          profittaker=max(min(profitlevel$Value[2], profitlevel$Value[3]), profitlevel$Value[4])
          #cat("Profit taker: ", profittaker)
          if(
            #this is roughly the same as VWEMA5<VWEMA20 or EMA5<EMA60 when down breakout happens (which is used in real trading). 
            BreakoutStructure$Bi$MIN[i+2+j]<=BreakoutStructure$Bi$MIN[i+2+j-2] && 
            nrow(breakema60)!=0
          ){
            sellP=profittaker
            sellReason="takeprofit"
            sellRefDate=profitlevel$Date[which(profitlevel$Value==profittaker)]
            cat("Profittaker was triggered. Close position.", "sellRefDate:", sellRefDate, "value:",sellP, "\n")
            j=j-2
            break}
          else{if((1+2+j)>=nrow(BreakoutStructure$Bi)){cat("No profit should be taken yet.", "sellRefDate:", BreakoutStructure$Bi[1+2+j-2,"BiEndD"], "value:", profittaker, "\n");break}else{j=j+2}
            #cat("move on to: ",j,"\n")
          }
        }
      }
      break}
    else{i=i-1}
  }
}




MACDThreeLineTest<-function(CombData, specifyDate=NULL){
  Data_macd<-PricedataMACD(CombData) #calculate the MACD
  Data_MF<-PricedataMoneyFlow(CombData)
  Data_MFI<-PricedataMFI(CombData)
  Data_EMA60<-FuncEMA60(CombData)
  
  #Make sure to start from the date on which all data have no NA
  minimumdate=map(list(CombData, Data_macd, Data_MF,Data_MFI,Data_EMA60), ~.x$Date)%>%Reduce(intersect, .)%>%first()
  SBPStr<-ChanLunStr(CombData)
  Bi<-SBPStr$Bi
  BiPlanetStr<-SBPStr$BiPlanetStr
  Bi=filter(Bi,BiStartD>=minimumdate)
  
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
      mfema1 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1,"BiEndD"])
      mfema2 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      
      minMF<-list(mf_div1,mf_div2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = FALSE)[1:3]))
      minMFI<-list(mfi_div1,mfi_div2)%>%map(~min(.x$MFI))
      #量比
      MFRatio <- list(mfema1, mfema2) %>%
        map(~ .x %>%summarise(Pos = sum(MoneyFlow_EMA[MoneyFlow_EMA > 0], na.rm = TRUE),
                              Neg = sum(MoneyFlow_EMA[MoneyFlow_EMA <= 0], na.rm = TRUE))
        )
      mfr<-1-MFRatio[[2]]$Neg/MFRatio[[1]]$Neg
      
      #MACDPower
      powerind=which(BiPlanetStr$BiEndD==BreakoutStructure$Bi[1+2,"BiEndD"])
      if(length(powerind)!=0){
        d1=CombData[max(1,which(CombData$Date==BiPlanetStr[powerind, 'BiStartD'])-500), "Date"]
        d2=BreakoutStructure$Bi[1+3,"BiEndD"]
        cat("Out2",BreakoutStructure$Bi[1+2,"BiEndD"],'\n')
        powerlist=MACDPower(filter(CombData,  Date>=d1 & Date<=d2),SBPStr = SBPStr)
        power_res=sum(as.numeric(strsplit(powerlist$Class, "")[[1]]))
        print(power_res)
      }else{power_res=NULL}
      
      if(falsebreakout==1){div=1}
      else if(
        (min(macd_div2$MACD) > min(macd_div1$MACD)) & ((0.999*minMF[[2]]>minMF[[1]]) | (minMFI[[2]]>minMFI[[1]]) |(mfr>0))
      ){div=1}else{div=0}
      
      
      
      res <- data.frame(
        Key = c(
          "MACD1", "MACD2", 
          "MF1", "MF2", 
          "MFI1", "MFI2", 
          "MFRatio1","MFRatio2",
          "Period Start", "Period End", 
          "Divergence", 'Macdpower', "False Breakout"
        ),
        Value = c(
          min(macd_div1$MACD), min(macd_div2$MACD), 
          minMF[[1]], 0.999*minMF[[2]], 
          minMFI[[1]], minMFI[[2]], 
          MFRatio[[1]]$Neg,MFRatio[[2]]$Neg,
          BreakoutStructure$Bi[1, "BiStartD"], 
          BreakoutStructure$Bi[1 + 2, "BiEndD"],
          as.logical(div), ifelse( !is.null(power_res) && power_res > 5, TRUE, FALSE), falsebreakout
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

















