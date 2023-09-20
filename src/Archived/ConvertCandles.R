#This files shows a general way of converting candlestick of one interval to another bigger interval. The following example shows 5F to 30F

fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/NQ5F_20200320.csv")
NQ5F<-read.csv(file = fileloc, header=T)

indices<-minute(as.POSIXct(NQ5F$Index))
NQ5F$mins<-indices


i<-1
j<-1
newData<-data.frame()
while(i+j <=nrow(NQ5F)){
  if(NQ5F[i+j, "mins"] %in% c(0, 30)){
    Index<-NQ5F[i, "Index"]
    Open<-NQ5F[i, "NQH0.Open"]
    High<-max(NQ5F[i:(i+j-1), "NQH0.High"])
    Low<-min(NQ5F[i:(i+j-1), "NQH0.Low"])
    Close<-NQ5F[(i+j-1), "NQH0.Close"]
    Volume<-sum(NQ5F[i:(i+j-1), "NQH0.Volume"])
    WAP<-NQ5F[(i+j-1), "NQH0.WAP"]
    hasGaps<-NQ5F[(i+j-1), "NQH0.hasGaps"]
    Count<-sum(NQ5F[i:(i+j-1), "NQH0.Count"])
    newData<-rbind(newData, data.frame(Index, NQH0.Open=Open, NQH0.High=High, NQH0.Low=Low, NQH0.Close=Close, NQH0.Volume=Volume,
                                       NQH0.WAP=WAP, NQH0.hasGaps=hasGaps, NQH0.Count=Count))
    i<-i+j
    j<-1
  }else{j<-j+1}
  
  if(i+j>nrow(NQ5F)){
    Index<-NQ5F[i, "Index"]
    Open<-NQ5F[i, "NQH0.Open"]
    High<-max(NQ5F[i:(i+j-1), "NQH0.High"])
    Low<-min(NQ5F[i:(i+j-1), "NQH0.Low"])
    Close<-NQ5F[(i+j-1), "NQH0.Close"]
    Volume<-sum(NQ5F[i:(i+j-1), "NQH0.Volume"])
    WAP<-NQ5F[(i+j-1), "NQH0.WAP"]
    hasGaps<-NQ5F[(i+j-1), "NQH0.hasGaps"]
    Count<-sum(NQ5F[i:(i+j-1), "NQH0.Count"])
    newData<-rbind(newData, data.frame(Index, NQH0.Open=Open, NQH0.High=High, NQH0.Low=Low, NQH0.Close=Close, NQH0.Volume=Volume,
                                       NQH0.WAP=WAP, NQH0.hasGaps=hasGaps, NQH0.Count=Count))
    break
  }
}

head(newData, 20)
tail(newData)





