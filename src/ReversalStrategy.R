Pricedata=NQ30FContinuous
Data_macd<-PricedataMACD(Pricedata) #calculate the MACD
Data_MF<-PricedataMoneyFlow(Pricedata)

SBPStr<-ChanLunStr(Pricedata)
StarData<-SBPStr$StarData
Bi<-SBPStr$Bi
BiPlanetStr<-SBPStr$BiPlanetStr

PL=data.frame(Date=NA, buyP=0, sellP=0,Profit=0)
i=1
while(i<=(nrow(Bi)-4)){
  if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
    ind=which(Pricedata$Date>=Bi$BiStartD[i+3] & Pricedata$Date<=Bi$BiEndD[i+3] & Pricedata$High>=Bi$MAX[i+2])%>%first()
    #cat("BiStartD:",Bi$BiStartD[i+2],'\n')

    #check MACD reversal
    macd_rev1=subset(Data_macd, Date>=Bi$BiStartD[i+1] & Date<=Bi$BiEndD[i+1])
    macd_rev2=subset(Data_macd, Date>=Bi$BiStartD[i+3] & Date<=Bi$BiEndD[i+3])
    maxMACD=list(macd_rev1,macd_rev2)%>%map(~max(.x$MACD))

    mf_rev1=subset(Data_MF, Date>=Bi$BiStartD[i+1] & Date<=Bi$BiEndD[i+1])
    mf_rev2=subset(Data_MF, Date>=Bi$BiStartD[i+3] & Date<=Bi$BiEndD[i+3])
    maxMF=list(mf_rev1,mf_rev2)%>%map(~max(.x$MoneyFlow))
    maxMF_EMA=list(mf_rev1,mf_rev2)%>%map(~max(.x$MoneyFlow_EMA))
    
    if(maxMACD[[2]]>0.98*maxMACD[[1]] & 
       (maxMF[[2]]>maxMF[[1]] | maxMF_EMA[[2]]>maxMF_EMA[[1]])){rev=1}else{rev=0}
    #cat("rev",rev,'\n')
    
    #check MACD divergence
    macd_div1 <- subset(Data_macd,Date>=Bi$BiStartD[i] & Date<=Bi$BiEndD[i])
    macd_div2 <- subset(Data_macd,Date>=Bi$BiStartD[i+2] & Date<=Bi$BiEndD[i+2])
    minMACD<-list(macd_div1,macd_div2)%>%map(~min(.x$MACD))
    minDEA<-list(macd_div1,macd_div2)%>%map(~min(.x$DEA))
    
    mf_div1 <- subset(Data_MF,Date>=Bi$BiStartD[i] & Date<=Bi$BiEndD[i])
    mf_div2 <- subset(Data_MF,Date>=Bi$BiStartD[i+2] & Date<=Bi$BiEndD[i+2])
    minMF<-list(mf_div1,mf_div2)%>%map(~min(.x$MoneyFlow))
    minMF_EMA=list(mf_div1,mf_div2)%>%map(~min(.x$MoneyFlow_EMA))
    
    if(minMACD[[2]]>minMACD[[1]] | minDEA[[2]]>minDEA[[1]]){div=1}else{div=0}
    
    #cat("div",div,'\n')
    
    ordertime=ymd_hms(Pricedata$Date[ind], tz="America/Toronto")%>%hour()
    #cat('order time:',ordertime,'\n')
    # MACD 和 MFI 创新高，时间7点以后，买入
    if(rev==1 & div==1 & ordertime>=7 & ordertime<=23){
      buyP=Pricedata[ind,"Close"]
      #cat("bought price:", buyP,'\n')
      #cat("bought time:", Pricedata[ind,"Date"],'\n')
    }
    else{i=i+1;next}  
    
    j=2
    stoploss=Bi$MIN[i+2] #止损在笔2的最低点
    
    while((i+2+j)<=nrow(Bi)){
      if(Bi$MAX[i+3]>=Bi$MAX[i+2+j]){ #如果笔4及以后的下降笔最高点小于笔3最高点，也就是在笔3区间内盘整
        pricebreak1=subset(Pricedata, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%mutate(Ret=log(Close/Open))%>%summarize(min=min(Ret))%>%as.numeric()
        pricebreak2=subset(Pricedata, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%mutate(Ret=log(Close/Open))%>%summarize(min=min(Ret))%>%as.numeric()
        
        macdbreak1=subset(Data_macd, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%summarize(min=min(MACD))%>%as.numeric()
        macdbreak2=subset(Data_macd, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%summarize(min=min(MACD))%>%as.numeric()
        
        mfbreak1=subset(Data_MF, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%summarize(min=min(MoneyFlow))%>%as.numeric()
        mfbreak2=subset(Data_MF, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%summarize(min=min(MoneyFlow))%>%as.numeric()
        
        if(Bi$MIN[i+2+j]<=stoploss ){ #任何时候下降笔破止损就卖出
          sellP=stoploss
          #cat("止损:",sellP,'\n')
          break}
        else if(pricebreak2<=1.2*pricebreak1 & macdbreak2<=2*macdbreak1 & mfbreak2<=mfbreak1){ #如果不破止损，但加速下跌，保本
          sellP=Bi$MIN[i+2+j]
          #cat("止损:",sellP,'\n')
          break
        }
        else{j=j+2} #以上都不满足则继续笔3区间震荡
      }
      else{ #如果突破笔3最高点，则止盈开始。止盈位是前低或者进场k线的最低点，取最大
        stoploss=max(Pricedata[ind,"Low"], Bi$MIN[i+2+j-2])
        #cat("Profit taker: ", stoploss)
        if(Bi$MIN[i+2+j]<=stoploss){
          sellP=stoploss
          #cat("sellP:",sellP,"\n")
          break}
        else{j=j+2
        #cat("move on to: ",j,"\n")
        }
      }
    }
    i=i+2+j-1 #往回数一笔，因为后面i=i+1
    PL=rbind(PL, data.frame(Date=Pricedata[ind,"Date"], buyP, sellP, Profit=sellP-buyP))
  }
  i=i+1
  #cat("Restart from i: ", i,"\n")
}


PL=PL[2:nrow(PL),]
sum(PL$Profit)

pos=PL%>%filter(Profit > 0)%>%select(Profit)
neg=PL%>%filter(Profit <= 0)%>%select(Profit)
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
