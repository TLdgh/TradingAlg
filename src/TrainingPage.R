Quantity <-c(2)
T..Price<-c(9214)
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="NQ",Quantity,T..Price,Leverage=2, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Data=list(SPY_daily=SPY_daily),Title="SPY_daily",PausePeriod = 4)

#NQ:
ChartReplay(Data=list(NQ4HContinuous=NQ4HContinuous, NQContinuous=NQContinuous, NQWContinuous=NQWContinuous), PausePeriod = 4)


