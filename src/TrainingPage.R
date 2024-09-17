Quantity <-c(2, -1, 3, -1,-2,-1)
T..Price<-c(13.95, 17.34, 21.97,23.05, 21.97,21.29)
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
            StartDate = "2004-03-19")

ChartReplay(Data = list(XLK_daily=XLK_daily), 
            StartDate = "2010-05-26")
1181

