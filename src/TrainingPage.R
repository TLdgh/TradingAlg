Quantity <-c(2)
T..Price<-c(9214)
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="NQ",Quantity,T..Price,Leverage=2, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Pricedata=SPY_daily,Title="SPY_daily", StartCandle=4454, PausePeriod = 7, UerInput = "N")

178+176+530+806+2056+882-40+1436+48

#NQ:
ChartReplay(Pricedata=NQ5FContinuous, AuxillaryData=NQ30FContinuous, 
            StartCandle=4542, PausePeriod=5, UerInput = "N")

x<-c(0)
for(i in 2:10000){
  x[i]<-sample(c(-1,1), 1, prob = c(0.45,0.55))+x[i-1]
}
plot(x, type="lines")