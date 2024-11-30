SBPStr<-ChanLunStr(Pricedata)
Data_macd<-PricedataMACD(Pricedata) #calculate the MACD
Data_MF<-PricedataMoneyFlow(Pricedata)
StarData<-SBPStr$StarData
Bi<-SBPStr$Bi
BiPlanetStr<-SBPStr$BiPlanetStr

PL=data.frame(Date=NA, buyP=0, sellP=0,Profit=0)
i=1
while(i<=(nrow(Bi)-4)){
  if(Bi$SLOPE[i]==-1 & Bi$MAX[i+2]<Bi$MAX[i] & Bi$MAX[i+2]<Bi$MAX[i+3]){
    ind=which(Pricedata$Date>=Bi$BiStartD[i+3] & Pricedata$Date<=Bi$BiEndD[i+3] & Pricedata$High>=Bi$MAX[i+2])%>%first()
    
    macd1=subset(Data_macd, Date>=Bi$BiStartD[i+1] & Date<=Bi$BiEndD[i+1])%>%summarize(max=max(MACD))%>%as.numeric()
    macd2=subset(Data_macd, Date>=Bi$BiStartD[i+3] & Date<=Bi$BiEndD[i+3])%>%summarize(max=max(MACD))%>%as.numeric()
    
    mf1=subset(Data_MF, Date>=Bi$BiStartD[i+1] & Date<=Bi$BiEndD[i+1])%>%summarize(max=max(MoneyFlow))%>%as.numeric()
    mf2=subset(Data_MF, Date>=Bi$BiStartD[i+3] & Date<=Bi$BiEndD[i+3])%>%summarize(max=max(MoneyFlow))%>%as.numeric()
    
    #check MACD divergence
    A1_interval <- subset(Data_macd,Date>=Bi$BiStartD[i] & Date<=Bi$BiEndD[i])
    A2_interval <- subset(Data_macd,Date>=Bi$BiStartD[i+2] & Date<=Bi$BiEndD[i+2])
    minMACD<-list(A1_interval,A2_interval)%>%map(~min(.x$MACD))
    minDEA<-list(A1_interval,A2_interval)%>%map(~min(.x$DEA))
    if(minMACD[[2]]>minMACD[[1]] | minDEA[[2]]>minDEA[[1]]){div=1}else{div=0}
    
    #cat("div",div,'\n')
    
    ordertime=ymd_hms(Pricedata$Date[ind], tz="America/Toronto")%>%hour()
    if(macd2>=macd1*0.98 & mf2>=mf1 & div==1 & ordertime>=7 & ordertime<=23){
      buyP=Pricedata[ind,"Close"]
      #cat("bought price:", buyP,'\n')
    }
    else{i=i+1;next}  
    
    j=2
    stoploss=Bi$MIN[i+2]
    
    while((i+2+j)<=nrow(Bi)){
      if(Bi$MAX[i+3]>=Bi$MAX[i+2+j]){
        pricebreak1=subset(Pricedata, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%mutate(Ret=log(Close/Open))%>%summarize(min=min(Ret))%>%as.numeric()
        pricebreak2=subset(Pricedata, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%mutate(Ret=log(Close/Open))%>%summarize(min=min(Ret))%>%as.numeric()
        
        macdbreak1=subset(Data_macd, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%summarize(min=min(MACD))%>%as.numeric()
        macdbreak2=subset(Data_macd, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%summarize(min=min(MACD))%>%as.numeric()
        
        mfbreak1=subset(Data_MF, Date>=Bi$BiStartD[i] & Date<=Bi$BiStartD[i+2+j])%>%summarize(min=min(MoneyFlow))%>%as.numeric()
        mfbreak2=subset(Data_MF, Date>=Bi$BiStartD[i+2+j] & Date<=Bi$BiEndD[i+2+j])%>%summarize(min=min(MoneyFlow))%>%as.numeric()
        
        if(Bi$MIN[i+2+j]<=stoploss ){
          sellP=stoploss
          #cat("止损:",sellP,'\n')
          break}
        else if(pricebreak2<=1.2*pricebreak1 & macdbreak2<=2*macdbreak1 & mfbreak2<=mfbreak1){
          sellP=Bi$MIN[i+2+j]
          #cat("止损:",sellP,'\n')
          break
        }
        else{j=j+2}
      }
      else{
        stoploss=max(Pricedata[ind,"Low"], Bi$MIN[i+2+j-2])
        #cat("new stoploss: ", stoploss) #this is the new stoploss, i.e. the profit taker
        if(Bi$MIN[i+2+j]<=stoploss){
          sellP=stoploss
          #cat("sellP:",sellP,"\n")
          break}
        else{j=j+2
        #cat("move on to: ",j,"\n")
        }
      }
    }
    i=i+2+j-1
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
