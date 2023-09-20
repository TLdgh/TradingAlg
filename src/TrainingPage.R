Quantity <-c(2, -2, 2, -1,  3, -2, -1, -1)
T..Price<-c(9329, 9353,9370, 9412, 9421, 9459, 9445, 9439)
Code<-sapply(Quantity, function(x) ifelse(x>0, "O","C"))     
data<-data.frame(Symbol="NQ",Quantity,T..Price,Leverage=2, Comm.Fee=0,Code)
PnL(data)



#SPY:
ChartReplay(Pricedata=SPY_daily,Title="SPY_daily", StartCandle=4454, PausePeriod = 7, UerInput = "N")

22583.75+78551+19613+1193.5

#NQ:
ChartReplay(Pricedata=subset(NQ5FContinuous, Date>="2019-12-24 09:00:00"),
            AuxillaryData=NQ30FContinuous, 
            StartCandle=2300, PausePeriod=7, UerInput = "N")


204+122+34+204-8+1900+560-40-36+244+300+62+756+280+124+518
