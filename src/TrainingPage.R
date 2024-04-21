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

