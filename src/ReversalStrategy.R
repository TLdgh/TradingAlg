CombData=NQ30FContinuous
Data_macd<-PricedataMACD(CombData) #calculate the MACD
Data_MF<-PricedataMoneyFlow(CombData)
Data_EMA60<-FuncEMA60(CombData)
Data_EMA30<-FuncEMA30(CombData)

SBPStr<-ChanLunStr(CombData)
Bi<-SBPStr$Bi

PL_test=data.frame(Date=NA, buyP=0, stoploss=0, sellP=0,Profit=0, sellReason=NA,sellRefDate=NA)
i=1
while(i<=(nrow(Bi)-4)){
  if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
    start_ind=Bi$BiStartD[i]
    end_ind=Bi$BiEndD[i+3]
    
    BreakoutStructure=list(Bi=filter(Bi,BiStartD>=start_ind, BiEndD<=end_ind), 
                           Price=filter(CombData,Date>=start_ind, Date<=end_ind),
                           MACD=filter(Data_macd,Date>=start_ind, Date<=end_ind),
                           MF=filter(Data_MF,Date>=start_ind, Date<=end_ind)
    )
    
    ind1=which(BreakoutStructure$Price$Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & BreakoutStructure$Price$High>=BreakoutStructure$Bi[1+2,"MAX"])%>%first()
    #cat("BiStartD:",Bi$BiStartD[i+2],'\n')
    
    #check MACD reversal
    macd_rev1=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
    macd_rev1_bygroup=macd_rev1%>%split_interval()#group the macd into pos and neg values
    macd_rev2=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
    revindex1 <- which(sapply(macd_rev1_bygroup, function(df) any(df$MACD>0))) #get the index of the df whose MACD>0
    if(length(revindex1)!=0){lastMaxMacd=max(macd_rev1_bygroup[[last(revindex1)]]$MACD)} #the max of the last positive MACD group
    
    mf_rev1=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
    mf_rev2=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
    maxMF=list(mf_rev1,mf_rev2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = TRUE)[1:3]))
    maxMF_EMA=list(mf_rev1,mf_rev2)%>%map(~max(.x$MoneyFlow_EMA))
    
    if(length(revindex1)==0){rev=1}
    else if((length(revindex1)!=0 & max(macd_rev2$MACD) >= 0.98*lastMaxMacd) &
            (maxMF[[2]]>=maxMF[[1]])
    ){rev=1}else{rev=0}
    #cat("rev",rev,'\n')
    
    #check MACD divergence
    macd_div1 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
    macd_div1_bygroup=macd_div1%>%split_interval()
    macd_div2 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
    divindex1 <- which(sapply(macd_div1_bygroup, function(df) any(df$MACD<0)))
    if(length(divindex1)!=0){lastMinMacd=min(macd_div1_bygroup[[last(divindex1)]]$MACD)} #the min of the last negative MACD group
    
    #假跌破
    falsebreakout=ifelse((min(macd_div2$MACD) < 2*min(macd_div1$MACD)) & (2*max(macd_div2$MACD) < max(macd_rev2$MACD)), 1, 0)
    
    mf_div1 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
    mf_div2 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
    minMF<-list(mf_div1,mf_div2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = FALSE)[1:3]))
    minMF_EMA=list(mf_div1,mf_div2)%>%map(~min(.x$MoneyFlow_EMA))
    
    if(length(divindex1)==0 | falsebreakout==1){div=1}
    else if((length(divindex1)!=0 & min(macd_div2$MACD) > lastMinMacd) &
            (minMF[[2]]>minMF[[1]])
    ){div=1}else{div=0}
    
    #cat("div",div,'\n')
    
    ordertime=ymd_hms(BreakoutStructure$Price[ind1, "Date"], tz="America/Toronto")%>%hour()
    #cat('order time:',ordertime,'\n')
    # MACD 和 MFI 创新高，时间7点以后，买入
    if(rev==1 & div==1 & ordertime>=6 & ordertime<=23){
      target_date=ifelse(length(revindex1)!=0, macd_rev2[first(which(macd_rev2$MACD>lastMaxMacd)),"Date"], macd_rev2[first(which(macd_rev2$MACD>0)),"Date"])
      ind2=which(BreakoutStructure$Price$Date==target_date)#如果笔12没有MACD>0，则是后者
      buyP=min(BreakoutStructure$Price[c(ind1, ind2), "Close"])
      
      #cat("bought price:", buyP,'\n')
      #cat("bought time:", min(BreakoutStructure$Price[c(ind1, ind2),"Date"]),'\n')
    }
    else{i=i+1;next}  
    
    j=2
    stoploss=Bi$MIN[i+2] #止损在笔2的最低点
    profittaker=0
    
    while((i+2+j)<=nrow(Bi)){
      if(Bi$MAX[i+3]>=Bi$MAX[i+2+j] & profittaker==0){ #如果笔4及以后的下降笔最高点小于笔3最高点，也就是在笔3区间内盘整
        # Calculate minimum return and corresponding price break date
        price_subset <- subset(CombData, Date >= Bi$BiStartD[i + 1] & Date <= Bi$BiStartD[i + 2 + j])
        minRet <- min(log(price_subset$Close / price_subset$Open)) # Compute min return directly
        pricebreak <- subset(CombData, Date >= Bi$BiStartD[i + 2 + j] & Date <= Bi$BiEndD[i + 2 + j])
        pbindex <- which(log(pricebreak$Close / pricebreak$Open) < 2 * minRet)[1] # Get first index
        pbdate <- if (!is.na(pbindex)) pricebreak$Date[pbindex] else NA
        
        # Calculate minimum MACD and corresponding MACD break date
        macd_subset <- subset(Data_macd, Date >= Bi$BiStartD[i + 1] & Date <= Bi$BiStartD[i + 2 + j])
        minMacd <- min(macd_subset$MACD[2:nrow(macd_subset)]) # Compute min MACD directly
        macdbreak <- subset(Data_macd, Date >= Bi$BiStartD[i + 2 + j] & Date <= Bi$BiEndD[i + 2 + j])
        mbindex <- which(macdbreak$MACD < 1.35 * minMacd)[1]
        mbdate <- if (!is.na(mbindex)) macdbreak$Date[mbindex] else NA
        
        # Calculate minimum MoneyFlow and corresponding MoneyFlow break date
        mf_subset <- subset(Data_MF, Date >= Bi$BiStartD[i + 1] & Date <= Bi$BiStartD[i + 2 + j])
        minMF <- min(mf_subset$MoneyFlow[2:nrow(mf_subset)]) # Compute min MoneyFlow directly
        mfbreak <- subset(Data_MF, Date >= Bi$BiStartD[i + 2 + j] & Date <= Bi$BiEndD[i + 2 + j])
        mfbindex <- which(mfbreak$MoneyFlow < minMF)
        mfbindex <- ifelse(length(mfbindex)>=2, mfbindex[2], NA)
        mfbdate <- if (!is.na(mfbindex)) mfbreak$Date[mfbindex] else NA
        
        # Consolidate clear positions
        ClearPosition <- na.omit(c(pbdate, mbdate, mfbdate))
        
        if(length(ClearPosition)==0 & Bi$MIN[i+2+j]<=stoploss){ #任何时候下降笔破止损就卖出
          sellP=stoploss
          sellReason="stoploss"
          j=j-2 #go back at least 2 steps to restart with at least three lines.
          sellRefDate=Bi$BiEndD[i+2]
          break}
        else if(length(ClearPosition)!=0){ #加速下跌，保本。如果不破止损就提前走，否则止损
          accP=CombData[which(CombData$Date==min(ClearPosition)),"Close"]
          if(accP<Data_EMA60[which(Data_EMA60$Date==min(ClearPosition)),"EMA60"]){
            sellP=max(stoploss, accP)
            sellReason="acceDecrease"
            sellRefDate=min(ClearPosition)
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            #cat("止损:",sellP,'\n')
            break}else{j=j+2}
        }
        else{j=j+2} #以上都不满足则继续笔3区间震荡
      }
      else{ #如果突破笔3最高点，则止盈开始。止盈位是前低或者进场k线的最低点，取最大
        profittaker=max(mean(Bi$MIN[i+2],CombData[ind1, "Low"]), Bi$MIN[i+2+j-2]*0.999)
        #cat("Profit taker: ", profittaker)
        if(Bi$MIN[i+2+j]<=profittaker & 
           any(CombData[which(CombData$Date==Bi$BiEndD[i+2+j]),"Close"] < subset(Data_EMA60, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%select(EMA60))
        ){
          sellP=profittaker
          sellReason="takeprofit"
          sellRefDate=Bi$BiEndD[i+2+j-2]
          #cat("sellP:",sellP,"\n")
          j=j-2
          break}
        else{j=j+2
        #cat("move on to: ",j,"\n")
        }
      }
    }
    i=i+2+j-1 #往回数一笔，因为后面i=i+1
    PL_test=rbind(PL_test, data.frame(Date=BreakoutStructure$Price[ind1, "Date"], buyP, stoploss, sellP, Profit=sellP-buyP, sellReason,sellRefDate))
  }
  i=i+1
  #cat("Restart from i: ", i,"\n")
}


PL_test=PL_test[2:nrow(PL_test),]
sum(PL_test$Profit)

pos=PL_test%>%filter(Profit > 0)%>%select(Profit)
neg=PL_test%>%filter(Profit <= 0)%>%select(Profit)
df=bind_rows(pos,neg)%>%mutate(Index=1:n())
cat("success rate: ", nrow(pos)/nrow(df))


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









#This is the function to check the strategy at any customized time, or of the most recent breakout structure
LatestBreakout<-function(CombData, specifyDate=NULL){
  Data_macd<-PricedataMACD(CombData) #calculate the MACD
  Data_MF<-PricedataMoneyFlow(CombData)
  Data_EMA60<-FuncEMA60(CombData)
  
  SBPStr<-ChanLunStr(CombData)
  Bi<-SBPStr$Bi
  
  i=ifelse(is.null(specifyDate), nrow(Bi)-3, which(Bi$BiStartD==specifyDate))
  
  while(i>=1){
    if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
      start_ind=Bi$BiStartD[i]
      
      BreakoutStructure=list(Bi=filter(Bi,BiStartD>=start_ind), 
                             Price=filter(CombData,Date>=start_ind),
                             MACD=filter(Data_macd,Date>=start_ind),
                             MF=filter(Data_MF,Date>=start_ind)
      )
      
      ind1=which(BreakoutStructure$Price$Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & BreakoutStructure$Price$High>=BreakoutStructure$Bi[1+2,"MAX"])%>%first()
      
      #check MACD reversal
      macd_rev1=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      macd_rev1_bygroup=macd_rev1%>%split_interval()#group the macd into pos and neg values
      macd_rev2=subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
      revindex1 <- which(sapply(macd_rev1_bygroup, function(df) any(df$MACD>0))) #get the index of the df whose MACD>0
      if(length(revindex1)!=0){lastMaxMacd=max(macd_rev1_bygroup[[last(revindex1)]]$MACD)} #the max of the last positive MACD group
      
      mf_rev1=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      mf_rev2=subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+3,"BiStartD"] & Date<=BreakoutStructure$Bi[1+3,"BiEndD"])
      maxMF=list(mf_rev1,mf_rev2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = TRUE)[1:3]))
      maxMF_EMA=list(mf_rev1,mf_rev2)%>%map(~max(.x$MoneyFlow_EMA))
      
      if(length(revindex1)==0){rev=1}
      else if((length(revindex1)!=0 & max(macd_rev2$MACD) >= 0.98*lastMaxMacd) &
              (maxMF[[2]]>=maxMF[[1]])
      ){rev=1}else{rev=0}
      cat("rev:",rev,'rev start date:',BreakoutStructure$Bi[1+1,"BiStartD"] ,'\n')
      
      #check MACD divergence
      macd_div1 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
      macd_div1_bygroup=macd_div1%>%split_interval()
      macd_div2 <- subset(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      divindex1 <- which(sapply(macd_div1_bygroup, function(df) any(df$MACD<0)))
      if(length(divindex1)!=0){lastMinMacd=min(macd_div1_bygroup[[last(divindex1)]]$MACD)} #the min of the last negative MACD group
      
      #假跌破
      falsebreakout=ifelse((min(macd_div2$MACD) < 2*min(macd_div1$MACD)) & (2*max(macd_div2$MACD) < max(macd_rev2$MACD)), 1, 0)
      
      mf_div1 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1,"BiStartD"] & Date<=BreakoutStructure$Bi[1+1,"BiEndD"])
      mf_div2 <- subset(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2,"BiEndD"])
      minMF<-list(mf_div1,mf_div2)%>%map(~mean(sort(.x$MoneyFlow, decreasing = FALSE)[1:3]))
      minMF_EMA=list(mf_div1,mf_div2)%>%map(~min(.x$MoneyFlow_EMA))
      
      if(length(divindex1)==0 | falsebreakout==1){div=1}
      else if((length(divindex1)!=0 & min(macd_div2$MACD) > lastMinMacd) &
              (minMF[[2]]>minMF[[1]])
      ){div=1}else{div=0}
      
      cat("div",div,'div start date:',BreakoutStructure$Bi[1,"BiStartD"] ,'\n')
      
      ordertime=ymd_hms(BreakoutStructure$Price[ind1, "Date"], tz="America/Toronto")%>%hour()
      #cat('order time:',ordertime,'\n')
      # MACD 和 MFI 创新高，时间7点以后，买入
      if(rev==1 & div==1 & ordertime>=6 & ordertime<=23){
        target_date=ifelse(length(revindex1)!=0, macd_rev2[first(which(macd_rev2$MACD>lastMaxMacd)),"Date"], macd_rev2[first(which(macd_rev2$MACD>0)),"Date"])
        ind2=which(BreakoutStructure$Price$Date==target_date)#如果笔12没有MACD>0，则是后者
        buyP=min(BreakoutStructure$Price[c(ind1, ind2), "Close"])
        cat("bought price:", buyP, "bought time:", min(BreakoutStructure$Price[c(ind1, ind2),"Date"]), '\n')
      }
      
      j=2
      stoploss=BreakoutStructure$Bi[1+2,"MIN"] #止损在笔2的最低点
      profittaker=0
      
      while((1+2+j)<=nrow(BreakoutStructure$Bi)){
        if(BreakoutStructure$Bi[1+3,"MAX"]>=BreakoutStructure$Bi[1+2+j,"MAX"] & profittaker==0){ #如果笔4及以后的下降笔最高点小于笔3最高点，也就是在笔3区间内盘整
          # Calculate minimum return and corresponding price break date
          price_subset <- filter(BreakoutStructure$Price, Date>=BreakoutStructure$Bi[1+1,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiStartD"])
          minRet <- min(log(price_subset$Close / price_subset$Open)) # Compute min return directly
          pricebreak <- filter(BreakoutStructure$Price, Date>=BreakoutStructure$Bi[1+2+j,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiEndD"])
          pbindex <- which(log(pricebreak$Close / pricebreak$Open) < 2 * minRet)[1] # Get first index
          pbdate <- if (!is.na(pbindex)) pricebreak$Date[pbindex] else NA
          
          # Calculate minimum MACD and corresponding MACD break date
          macd_subset <- filter(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+1,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiStartD"])
          minMacd <- min(macd_subset$MACD[2:nrow(macd_subset)]) # Compute min MACD directly
          macdbreak <- filter(BreakoutStructure$MACD, Date>=BreakoutStructure$Bi[1+2+j,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiEndD"])
          mbindex <- which(macdbreak$MACD < 1.35 * minMacd)[1]
          mbdate <- if (!is.na(mbindex)) macdbreak$Date[mbindex] else NA
          
          # Calculate minimum MoneyFlow and corresponding MoneyFlow break date
          mf_subset <- filter(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+1,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiStartD"])
          minMF <- min(mf_subset$MoneyFlow[2:nrow(mf_subset)]) # Compute min MoneyFlow directly
          mfbreak <- filter(BreakoutStructure$MF, Date>=BreakoutStructure$Bi[1+2+j,"BiStartD"], Date<=BreakoutStructure$Bi[1+2+j,"BiEndD"])
          mfbindex <- which(mfbreak$MoneyFlow < minMF)
          mfbindex <- ifelse(length(mfbindex)>=2, mfbindex[2], NA)
          mfbdate <- if (!is.na(mfbindex)) mfbreak$Date[mfbindex] else NA
          
          # Consolidate clear positions
          ClearPosition <- na.omit(c(pbdate, mbdate, mfbdate))
          
          if(length(ClearPosition)==0 & BreakoutStructure$Bi[1+2+j,"MIN"]<=stoploss){ #任何时候下降笔破止损就卖出
            sellP=stoploss
            sellReason="stoploss"
            j=j-2 #go back at least 2 steps to restart with at least three lines.
            sellRefDate=BreakoutStructure$Bi[1+2,"BiEndD"]
            cat("stoploss was triggered!", "sellRefDate:", sellRefDate, "value:", sellP, "\n")
            break}
          else if(length(ClearPosition)!=0){ #加速下跌，保本。如果不破止损就提前走，否则止损
            accP=BreakoutStructure$Price[which(BreakoutStructure$Price$Date==min(ClearPosition)),"Close"]
            if(accP<Data_EMA60[which(Data_EMA60$Date==min(ClearPosition)),"EMA60"]){
              sellP=max(stoploss, accP)
              sellReason="acceDecrease"
              sellRefDate=min(ClearPosition)
              j=j-2 #go back at least 2 steps to restart with at least three lines.
              cat("acceDecrease. Exit immediately!", "sellRefDate:", sellRefDate, "value:", sellP, "\n")
              break}else{j=j+2}
          }
          else{if((1+2+j)>=nrow(BreakoutStructure$Bi)){cat("No stoploss triggered.", "sellRefDate:", BreakoutStructure$Bi[1+2,"BiEndD"], "value:", stoploss, "\n");break}else{j=j+2}} #以上都不满足则继续笔3区间震荡
        }
        else{ #如果突破笔3最高点，则止盈开始。止盈位是前低或者进场k线的最低点，取最大
          profittaker=max(mean(BreakoutStructure$Bi[1+2,"MIN"],BreakoutStructure$Price[ind1, "Low"]), BreakoutStructure$Bi[1+2+j-2,"MIN"]*0.999)
          #cat("Profit taker: ", profittaker)
          if(BreakoutStructure$Bi[1+2+j,"MIN"]<=profittaker & 
             any(BreakoutStructure$Price[which(BreakoutStructure$Price$Date==BreakoutStructure$Bi[1+2+j,"BiEndD"]),"Close"] < filter(Data_EMA60, Date>=BreakoutStructure$Bi[1+2+j,"BiStartD"] & Date<=BreakoutStructure$Bi[1+2+j,"BiEndD"])%>%select(EMA60))
          ){
            sellP=profittaker
            sellReason="takeprofit"
            sellRefDate=BreakoutStructure$Bi[1+2+j-2,"BiEndD"]
            #cat("sellP:",sellP,"\n")
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








