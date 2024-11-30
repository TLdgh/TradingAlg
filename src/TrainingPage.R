Quantity<-c(2, -1,-1,2,-2)
T..Price<-c(9553,9637,9658,9665,9661)
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="XLK",Quantity,T..Price,Leverage=2, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Data=list(SPY_daily=SPY_daily),StartDate = "2002-04-04",PausePeriod = 7)

#NQ:
ChartReplay(Data=list(NQ5FContinuous=NQ5FContinuous, 
                      NQ30FContinuous=NQ30FContinuous), 
            StartDate = "2020-10-05 05:00:00", UserInput = TRUE)

ChartReplay(Data=list(NQ4HContinuous=NQ4HContinuous,
                      NQ30FContinuous=NQ30FContinuous,
                      NQ5FContinuous=NQ5FContinuous), 
            StartDate = "2020-08-07 11:10:00",UserInput = TRUE)


#底背离测试
ChartReplay(Data=list(NQ4HContinuous=NQ4HContinuous,
                      NQ30FContinuous=NQ30FContinuous,
                      NQ5FContinuous=NQ5FContinuous), 
            StartDate = "2020-06-04 19:10:00",UserInput = TRUE)

ChartReplay(Data = list(XLK_daily=XLK_daily, XLK_weekly=XLK_weekly), 
            StartDate = "2016-02-10", PausePeriod = 7)

ChartReplay(Data = list(XLK_daily=XLK_daily), 
            StartDate = "2014-10-02", PausePeriod = 7)





-488-412