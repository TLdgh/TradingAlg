library(Quandl)
library(tidyquant)
library(plotly)
library(TTR)


######################################################### Step1: Import new data downloaded from IB_API. Data must have recent ones on the top rows, i.e. time should be descending.
#For intraday futures, we don't need to adjust data because we don't care futures contract rollover.
#Simply import the data and combine candles
Symb <- "NQ"
intv<- "30F"


Fut_intra<-read.csv(file = "/Users/tengli/R/Data/OriginalFuturesData/NQ/NQ30F.csv",header=T) 
Fut_intra<-Fut_intra[,1:6]
colnames(Fut_intra) <- c("Date", "Open", "High","Low", "Close", "Volume")
Fut_intra <- Fut_intra[order(Fut_intra$Date, decreasing = TRUE),]
head(Fut_intra)
tail(Fut_intra)


write.csv(Fut_intra, file="/Users/tengli/R/Data/NQ/NQ30F.csv", row.names = F) 

######################################################### Step2: Candle combination
#For combining the candles, go to Terminal, change directory
#cd CandleStick/CandleStickCombination/CandleStickCombination
#then enter csc Program.cs to get Program.exe, then enter mono Program.exe and enter the file path
#/Users/tengli/R/Data/Fut_intra
#the combined data will be saved in CandleStcikComb as output.csv. Rename it to NQComb_intra.

NQ30FComb <- read.csv("/Users/tengli/CandleStickComb/NQ/NQ30FComb.csv", header = T) 
NQ30FComb <- NQ30FComb[order(NQ30FComb$Date, decreasing = FALSE),]
head(NQ30FComb)
tail(NQ30FComb)

NQ30FComb<-subset(NQ30FComb, NQ30FComb$Date<="2021-08-20 06:05:00")
##########################################################################Price plotly
source("/Users/tengli/R/Script/StockPlotFunction.R") #everytime we run a function from a different script, we must run this command
StockChart(NQ30FComb, Title = "NQ30FComb")


#compare the MACD bar area
source("/Users/tengli/R/Script/MACDPower.R") #everytime we run a function from a different script, we must run this command
source("/Users/tengli/R/Script/ChanLunFunction.R") #everytime we run a function from a different script, we must run this command
StarData <- StarFunction(NQ5FComb)
Bi<-BiFunction(StarData)
Finalplanet <- as.data.frame(PlanetFunction(Bi))
Finalplanet <- subset(Finalplanet, PlanetHigh!=0)
Barstart1 <- as.POSIXct(StarData[which(StarData$Date==tail(Finalplanet,n=1)$PlanetStartD)-1, "Date"], tz="UTC") #this gives the start date of the incoming bi of the final planet
Barend1 <- as.POSIXct(tail(Finalplanet,n=1)$PlanetStartD, tz="UTC") #this gives the end date of the incoming bi of the final planet
Barstart2 <- as.POSIXct(tail(Finalplanet,n=1)$PlanetEndD, tz="UTC") #this gives the start date of the leaving bi of the final planet
Barend2 <- as.POSIXct(StarData[which(StarData$Date==tail(Finalplanet,n=1)$PlanetEndD)+1, "Date"], tz="UTC") #this gives the end date of the leaving bi of the final planet
MACDPower(NQ5FComb, Barstart1, Barend1, Barstart2, Barend2)


BarArea <- as.xts(NQ5FComb[,-1],order.by = as.POSIXct(NQ5FComb$Date))
BarArea<-BarArea["2021-08-17 03:50:00/2021-08-17 09:55:00"]   #choose the period in which you want to see the quick chart
BarArea%>%chartSeries(TA="addVo();addMACD()")


###MACD plot
yala_macd <- as.data.frame(MACD(Cl(YALA[order(YALA$Date, decreasing = F),]), nFast = 12, nSlow = 26, nSig = 9, 
                                maType = EMA, percent = FALSE))
yala_macd$MACD <- (yala_macd$macd - yala_macd$signal)*2
yala_macd$Date <- YALA[order(YALA$Date, decreasing = F),]$Date
colnames(yala_macd) <- c("DIFF", "DEA", "MACD", "Date")
head(yala_macd,35)
tail(yala_macd)
MACDChart <- ggplot(yala_macd)+ 
  geom_line(aes(x=index(yala_macd),y=DIFF),color="orange")+
  geom_line(aes(x=index(yala_macd), y=DEA), color = "blue")+
  geom_col(aes(x=index(yala_macd), y= MACD), color="grey")

#compare the bar area
subplot(StockChart(YALA), MACDChart, nrows=2, shareX = FALSE, heights = c(0.8, 0.2)) #combind the charts
Barstart1 <- as.POSIXct("2021-05-18 10:30:00", tz="UTC")
Barend1 <- as.POSIXct("2021-05-19 09:35:00", tz="UTC")
A1_interval <- subset(yala_macd,yala_macd$Date>= Barstart1 & yala_macd$Date<=Barend1)
area_neg1 <- 0
area_pos1 <- 0

for (i in 1:nrow(A1_interval)) {
  if (A1_interval[i,3]<0) {
    area_neg1 <- area_neg1+A1_interval[i,3]
  }else {
    area_pos1 <- area_pos1+A1_interval[i,3]
  }
}


Barstart2 <- as.POSIXct("2021-05-19 12:30:00", tz="UTC")
Barend2 <- as.POSIXct("2021-05-20 12:40:00", tz="UTC")
A2_interval <- subset(yala_macd,yala_macd$Date>= Barstart2 & yala_macd$Date<=Barend2)
area_neg2 <- 0
area_pos2 <- 0

for (i in 1:nrow(A2_interval)) {
  if (A2_interval[i,3]<0) {
    area_neg2 <- area_neg2+A2_interval[i,3]
  }else {
    area_pos2 <- area_pos2+A2_interval[i,3]
  }
}


cat("上涨能量背驰：", area_pos2<=area_pos1, "\n")
cat("下跌能量背驰：", area_neg2>=area_neg1)













