Quantity <-c(2)
T..Price<-c()
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="NQ",Quantity,T..Price,Leverage=2, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Data=list(SPY_daily=SPY_daily),Title="SPY_daily",PausePeriod = 4)

#NQ:
ChartReplay(Data=list(NQ30FContinuous=NQ30FContinuous, NQ4HContinuous=NQ4HContinuous, NQContinuous=NQContinuous), PausePeriod = 7)

ChartReplay(Data=list(NQ30FContinuous=NQ30FContinuous, NQ4HContinuous=NQ4HContinuous, NQContinuous=NQContinuous), 
            StartDate = "2020-02-18 02:00:00", PausePeriod = 7)


292+422+106+1518