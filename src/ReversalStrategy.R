testReverseStrategy<-function(CombData){
  #CombData=NQ4HContinuous
  Data_macd<-PricedataMACD(CombData) #calculate the MACD
  Data_MF<-PricedataMoneyFlow(CombData)
  Data_MFI<-PricedataMFI(CombData)
  Data_EMA60<-FuncEMA60(CombData)
  Data_EMA30<-FuncEMA30(CombData)
  Data_EMA20<-FuncEMA20(CombData)
  Data_EMA5<-FuncEMA5(CombData)
  
  minimumdate=map(list(CombData, Data_macd, Data_MF,Data_MFI,Data_EMA60,Data_EMA30,Data_EMA20,Data_EMA5), ~.x$Date[1:500])%>%Reduce(intersect, .)%>%first()
  SBPStr<-ChanLunStr(CombData)
  Bi<-SBPStr$Bi
  BiPlanetStr<-SBPStr$BiPlanetStr
  Bi=filter(Bi,BiStartD>=minimumdate)
  
  PL_test=data.frame(Date=NA, buyP=0, stoploss=0, sellP=0,Profit=0, sellReason=NA,sellRefDate=NA)
  i=1
  while(i<=(nrow(Bi)-4)){
    if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
      start_ind=Bi$BiStartD[i]
      end_ind=Bi$BiEndD[i+3]
      
      BreakoutStructure=list(Bi=filter(Bi,BiStartD>=start_ind, BiEndD<=end_ind), 
                             Price=filter(CombData,Date>=start_ind, Date<=end_ind),
                             MACD=filter(Data_macd,Date>=start_ind, Date<=end_ind),
                             MF=filter(Data_MF,Date>=start_ind, Date<=end_ind),
                             MFI=filter(Data_MFI,Date>=start_ind, Date<=end_ind)
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
      #cat("rev",rev,'\n')
      
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
      mfr<-if_else(MFRatio[[1]]$Neg != 0,
                   1 - MFRatio[[2]]$Neg / MFRatio[[1]]$Neg,
                   0)
      
      #MACDPower
      powerind=which(BiPlanetStr$BiEndD==BreakoutStructure$Bi[1+2,"BiEndD"])
      if(length(powerind)!=0){
        d1=CombData[max(1,which(CombData$Date==BiPlanetStr[powerind, 'BiStartD'])-500), "Date"]
        d2=BreakoutStructure$Bi[1+3,"BiEndD"]
        #cat("Out2",BreakoutStructure$Bi[1+2,"BiEndD"],'\n')
        powerlist=MACDPower(filter(CombData,  Date>=d1 & Date<=d2),SBPStr = SBPStr)
        power_res=sum(as.numeric(strsplit(powerlist$Class, "")[[1]]))
      }else{power_res=NULL}
      
      
      if(falsebreakout==1){div=1}
      else if(
        (min(macd_div2$MACD) > min(macd_div1$MACD)) & 
        ((0.999*minMF[[2]]>minMF[[1]]) | (minMFI[[2]]>minMFI[[1]]) | (mfr>0))     
      ){div=1}else{div=0}
      
      #cat("div",div,'\n')
      
      
      # MACD 和 MFI 创新高，时间7点以后，买入
      if( ((rev==1 & div==1)|( !is.null(power_res) && power_res > 5)) & 
          ((!all(is.na(ordertime)) && any(ordertime>=6 & ordertime<=23) ) | all(is.na(ordertime)) ) ){ #if is na, it means it's daily/weekly time
        
        #The target date is the latest date when both MACD and MF break high, and if this date is before the one when price breaks high, we have a better price and can enter earlier.
        target_date_macd=ifelse(length(revindex1)!=0, macd_rev2[first(which(macd_rev2$MACD>lastMaxMacd)),"Date"], macd_rev2[first(which(macd_rev2$MACD>0)),"Date"])
        target_date_mf=mf_rev2[which(mf_rev2$MoneyFlow>=max(mf_rev1$MoneyFlow)),"Date"]%>%first()
        target_date=max(target_date_macd,target_date_mf)
        ind2=which(timebreakhigh$Date==target_date)
        buyP=min(BreakoutStructure$Bi[1+2,"MAX"], timebreakhigh[ind2, "Close"])
        
        #cat("bought price:", buyP,'\n')
        #cat("bought time:", min(timebreakhigh[c(ind1, ind2),"Date"]),'\n')
      }
      else{i=i+1;next}  
      
      
      
      
      j=2
      stoploss=Bi$MIN[i+2] #止损在笔2的最低点
      profittaker=0
      
      while((i+2+j)<=nrow(Bi)){
        if(Bi$MAX[i+3]>=Bi$MAX[i+2+j]){ #如果笔4及以后的下降笔最高点小于笔3最高点，也就是在笔3区间内盘整
          
          testdata=list(Price=filter(CombData,Date>=start_ind & Date<=Bi$BiEndD[i+2+j]),
                        MACD=filter(Data_macd,Date>=start_ind & Date<=Bi$BiEndD[i+2+j]),
                        MF=filter(Data_MF,Date>=start_ind & Date<=Bi$BiEndD[i+2+j])
          )
          
          # Consolidate clear positions
          ClearPosition=getMins(testdata, 
                                d1=Bi[i+2+j-2,"BiStartD"], 
                                d2=Bi[i+2+j-2,"BiEndD"],
                                d3=Bi[i+2+j,"BiStartD"], 
                                d4=Bi[i+2+j,"BiEndD"])%>%keep(~ length(.x) > 0)
          
          if(length(ClearPosition)!=0){
            ClearPosition=ClearPosition%>%unlist()%>%sort()
            accP=CombData$Close[match(ClearPosition, CombData$Date)]
            accPind=accP<Data_EMA30$EMA30[match(ClearPosition, Data_EMA30$Date)]
            accPind=which(accPind==TRUE)
            # This checks if stoploss happens before accDecrease
            anylower=filter(CombData, Date>=Bi$BiStartD[i + 2 + j] & Date<=Bi$BiEndD[i + 2 + j] & Low<=stoploss)%>%first()
            if(nrow(anylower)==1 & ( !is.null(accPind) && length(accPind)>0 && ClearPosition[accPind[1]]>anylower$Date)){accP=NULL;accPind=NULL}
          }else{
            accP=NULL
            accPind=NULL
          }
          
          if((is.null(accPind) | ( !is.null(accPind) && length(accPind)<=1) ) &
             Bi$MIN[i+2+j]<=stoploss){ #任何时候下降笔破止损就卖出
            sellP=stoploss
            sellReason="stoploss"
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            sellRefDate=Bi$BiEndD[i+2]
            break}
          else if( !is.null(accPind) && length(accPind)>1){ #加速下跌，保本。如果不破止损就提前走，否则止损
            sellP=max(stoploss, accP[accPind[1]])
            sellReason=paste("acceDecrease:", names(ClearPosition[accPind[1]]))
            sellRefDate=ClearPosition[accPind[1]]%>%as.character()
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            #cat("止损:",sellP,'\n')
            break}
          else{j=j+2} #以上都不满足则继续笔3区间震荡
        }
        else{ #如果突破笔3最高点，则止盈开始。止盈位是前低或者进场k线的最低点，取最大
          begincheck<-filter(CombData, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j] & Close<=Bi$MIN[i+2+j-2])%>%
            first()
          
          if(nrow(begincheck)!=0){
            strucBreak <- map(list(data=CombData, ema5=Data_EMA5, ema20=Data_EMA20, ema60=Data_EMA60), ~ {
              filter(.x, Date >= begincheck$Date & Date <= Bi$BiEndD[i+2+j])
            })
            below60=which(strucBreak$data$Close<strucBreak$ema60$EMA60)%>%first()
            fivebelow20=which(strucBreak$ema5$EMA5<strucBreak$ema20$EMA20)%>%first()
            # cleanup
            strucBreak <- if (is_empty(below60) && is_empty(fivebelow20)) {
              slice(strucBreak$data, 0) # Zero-row data frame
            } else {
              min_index <- c(below60, fivebelow20) %>% discard(is_empty) %>% min()
              slice(strucBreak$data, min_index)
            }
            
            profitlevel=list(Date=c(Bi$BiEndD[i+2], Bi$BiEndD[i+2+j], Bi$BiEndD[i+2+j-2], strucBreak$Date), 
                             Value=c(Bi$MIN[i+2], mean(c(Bi$MIN[i+2],Bi$MAX[i+2])), Bi$MIN[i+2+j-2]*0.999, strucBreak$Close))
            
            profittaker=max(min(profitlevel$Value[2], profitlevel$Value[3]), profitlevel$Value[4])
            #cat("Profit taker: ", profittaker)
            if(Bi$MIN[i+2+j]<=Bi$MIN[i+2+j-2] && 
               nrow(strucBreak)!=0
            ){
              sellP=profittaker
              sellReason="takeprofit"
              sellRefDate=profitlevel$Date[which(profitlevel$Value==profittaker)]
              #cat("sellP:",sellP,"\n")
              j=j-2
              break}
            else{j=j+2}
          }else{j=j+2}
        }
      }
      i=i+2+j-1 #往回数一笔，因为后面i=i+1
      PL_test=rbind(PL_test, data.frame(Date=timebreakhigh[min(ind1, ind2), "Date"], buyP, stoploss, sellP, Profit=sellP-buyP, sellReason,sellRefDate))
    }
    i=i+1
    #cat("Restart from i: ", i,"\n")
  }
  res=PL_test[2:nrow(PL_test),]%>%summarise(Total=sum(Profit), Average=mean(Profit), SuccessRate=mean(Profit > 0) * 100)
  return(res)
}

result <- list(
  NQ4HContinuous=NQ4HContinuous,
  NQ2HContinuous=NQ2HContinuous,
  NQ1HContinuous=NQ1HContinuous,
  NQ30FContinuous=NQ30FContinuous
  #NQ5FContinuous=NQ5FContinuous
)%>%map_dfr(., testReverseStrategy, .id = "DataFrameName")
result











# Define bin size
bin_size <- 50

# Calculate the bin edges
min_edge <- floor(min(df$Profit) / bin_size) * bin_size
max_edge <- ceiling(max(df$Profit) / bin_size) * bin_size
bin_edges <- seq(min_edge, max_edge, by = bin_size)

# Plot histogram with explicit tick labels for bin edges
df%>%plot_ly(x = ~Profit,type = "histogram",
             xbins = list(start = min_edge,end = max_edge,size = bin_size),
             marker = list(line = list(color = "black",width = 1)))%>%
  layout(
    xaxis = list(
      tickvals = bin_edges,      # Set tick labels to the bin edges
      ticktext = bin_edges,      # Labels to display for the ticks
      tickmode = "array"         # Use an array for tick positions
    ),
    yaxis = list(title = "Count")
  )




#4H
#12020.572 73.91304

#2H
#11334.668 63.82979

#1H
#6039.709 57.74648

#30F:
#24279.289 60.36585

#5F
#32737.95 52.44596


d1='2019-12-01 09:00:00'
d2='2020-12-01 09:00:00'
d="2020-05-28 15:30:00"

ChartReplay(Data=list(NQ4HContinuous=NQ4HContinuous,
                      NQ30FContinuous=NQ30FContinuous), 
            StartDate=d ,UserInput = TRUE)

MACDThreeLineTest(subset(NQ30FContinuous,Date<=d))
MACDPower(subset(NQ30FContinuous,Date<=d), 'NQ30FContinuous')

LatestBreakout(subset(NQ30FContinuous,Date<=d))
LatestBreakout(subset(NQ4HContinuous,Date<='2020-01-08 10:00:00'))



