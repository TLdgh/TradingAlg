Quantity<-c(2,      2,    2,    -2,-4)
T..Price<-c(40.52,40.08,43.68,42.28,39.97)
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="XLK",Quantity,T..Price,Leverage=100, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Data=list(SPY_daily=SPY_daily),PausePeriod = 4)

#NQ:
ChartReplay(Data=list(NQ30FContinuous=NQ30FContinuous, NQ4HContinuous=NQ4HContinuous, NQContinuous=NQContinuous), PausePeriod = 7)

ChartReplay(Data=list(NQ30FContinuous=NQ30FContinuous, NQ4HContinuous=NQ4HContinuous, NQContinuous=NQContinuous), 
            StartDate = "2020-02-18 02:00:00", PausePeriod = 7)



ChartReplay(Data = list(XLK_daily=XLK_daily, XLK_weekly=XLK_weekly), 
            StartDate = "2016-02-10", PausePeriod = 7)

ChartReplay(Data = list(XLK_daily=XLK_daily), 
            StartDate = "2014-10-02", PausePeriod = 7)



-488-412