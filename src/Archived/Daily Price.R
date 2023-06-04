library(tidyverse)
library(tidyquant)
library(plotly)
library(TTR)

av_api_key("<77P2LJBQ51UX69O7>")
Symb <- "rgen"
intv<- "daily"
seriesT <- "close"
OutputSize <- "full"
PeriodStart_I<- as.POSIXct("2021-07-01", tz="UTC")
PeriodEnd_I<- as.POSIXct("2021-07-08", tz="UTC")

####Data must have recent ones on the top rows, i.e. time should be descending.
####For combining the candles, go to Terminal, change directory cd CandleStick/CandleStickCombination/CandleStickCombination
####then enter csc Program.cs to get Program.exe, then enter mono Program.exe and enter the file path /Users/tengli/R/Data/xxxx.csv
####the combined data will be saved in CandleStcikComb as output.csv. Rename it.

stock_daily <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_DAILY", outputsize=OutputSize)
Data_I <- as.data.frame(stock_daily[,-1])
colnames(Data_I) <- c("Date", "Open", "High","Low", "Close", "Volume")
head(Data_I)
tail(Data_I)

#Data_I <- subset(Data_I,Data_I$time>=PeriodStart_I & Data_I$time<= PeriodEnd_I) #subset only the period we want

nam <- paste(toupper(Symb),"D", sep = "")
fileloc <- paste("/Users/tengli/R/Data/US/", nam, ".csv", sep = "")
write.csv(Data_I, file = fileloc, row.names = F)

Combfileloc <- paste("/Users/tengli/CandleStickComb/US/", nam, "Comb.csv", sep = "")
CombData <- read.csv(Combfileloc, header = T) 
CombData <- CombData[order(CombData$Date, decreasing = FALSE),]
assign(nam,CombData)

##########################################################################Price plotly
source("/Users/tengli/R/Script/StockPlotFunction.R") #everytime we run a function from a different script, we must run this command
StockChart(Pricedata=CombData,Title=nam)

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


BarArea <- as.xts(CombData[,-1],order.by = as.POSIXct(CombData$Date))
BarArea<-BarArea["2021-08-17 03:50:00/2021-08-17 09:55:00"]   #choose the period in which you want to see the quick chart
BarArea%>%chartSeries(TA="addVo();addMACD()")





###MACD plot
stock_macd <- as.data.frame(MACD(Cl(Data_I[order(Data_I$Date, decreasing = F),]), nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE))
stock_macd$MACD <- (stock_macd$macd - stock_macd$signal)*2
stock_macd$Date <- Data_I[order(Data_I$Date, decreasing = F),]$Date
colnames(stock_macd) <- c("DIFF", "DEA", "MACD", "Date")
head(stock_macd,35)
tail(stock_macd)
ggplot(stock_macd)+ 
  geom_line(aes(x=index(stock_macd),y=DIFF),color="orange")+
  geom_line(aes(x=index(stock_macd), y=DEA), color = "blue")+
  geom_col(aes(x=index(stock_macd), y= MACD), color="grey")


#Or
ggplot(MACD_Intraday, aes(x =index(MACD_Intraday))) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = MACD_Intraday$macd, color = "MACD")) +
  geom_line(aes(y = MACD_Intraday$macd_signal, color = "Signal")) +
  geom_bar(aes(y = MACD_Intraday$macd_hist), stat = "identity", color = palette_light()[[1]])+
  scale_colour_manual("Series", values=c("MACD"="orange", "Signal"="blue"))



###RSI plot
ggplot(RSI_Daily, aes(x =Date)) + 
  geom_hline(yintercept = 70, color = palette_light()[[1]]) +
  geom_hline(yintercept = 30, color = palette_light()[[2]]) +
  geom_line(aes(y=rsi, color = "rsi")) 


### Indicators
Indicators <- matrix(0, ncol = 11)
colnames(Indicators)<- c("Ind_MA10", "Ind_MA20", "Ind_MA60", "Ind_MA200", "Ind_RSI", "Ind_MACD", 
                         "Resistence", "Support", "VolScale","AllTimeHigh","CandleMeasure")
Resistence <-270
Support <-256.69
VolScale <- last(Data_D$volume)[[1]]/mean(Data_D$volume)
AllTimeHigh<- 1
CandleMeasure <- 0

Indicators[1,1] <- if (SMA10_D[1,3]<=coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))) 1 else -1
Indicators[1,2] <- if (SMA20_D[1,3]<=coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))) 1 else -1
Indicators[1,3] <- if (SMA60_D[1,3]<=coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))) 1 else -1
Indicators[1,4] <- if (SMA200_D[1,3]<=coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))) 1 else -1
Indicators[1,7] <- if (coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))>=Resistence) 1 else 0
Indicators[1,8] <- if (coredata(last(Data_D[,grep(seriesT, colnames(Data_D))]))<=Support) -1 else 0
Indicators[1,9] <- VolScale
Indicators[1,10] <-AllTimeHigh
Indicators[1,11] <-CandleMeasure

if(RSI_Daily[RSI_Daily$Date==TargetDate,]$rsi <30){
  Indicators[1,5]=1} else if(RSI_Daily[RSI_Daily$Date==TargetDate,]$rsi >70){
    Indicators[1,5]=-1} else {Indicators[1,5]=0}

Ind_MACD_Matrix <- matrix(0, ncol=3)
Ind_MACD_Matrix[1,1]<-if(MACD_Daily[MACD_Daily$Date==TargetDate,]$macd > MACD_Daily[MACD_Daily$Date==TargetDate,]$macd_signal) 1 else -1
Ind_MACD_Matrix[1,2]<-if(MACD_Daily[MACD_Daily$Date==TargetDate,]$macd > 0) 1 else -1 
Ind_MACD_Matrix[1,3]<-if(MACD_Daily[MACD_Daily$Date==TargetDate,]$macd_signal>0) 1 else -1
Indicators[1,6] <- sum(Ind_MACD_Matrix)

print(Indicators)
cat("The total indicator is: ", sum(Indicators))


#####Bootstrap
SampleData<-c(coredata(stock_daily$open),coredata(stock_daily$high),coredata(stock_daily$low),coredata(stock_daily$close) )
nboot <- 1000
data <- matrix(sample(SampleData, size = length(SampleData)*nboot, replace = T), nrow = nboot)
BootData <- data.frame(Prob = rep(0,1,nboot), Mean = rep(0,1,nboot))
for (i in 1:nboot) {
  Fn <- ecdf(data[i,])
  BootData[i,1] <- 1-Fn(currentprice)
  BootData[i,2] <- mean(data[i,])
  
}
mean(BootData$Prob)
sd(BootData$Prob)
#hist(BootData$Prob)
mean(BootData$Mean)
sd(BootData$Mean)

cat("Estimated probability: ", sum(Indicators)*(mean(BootData$Prob)+2*sd(BootData$Prob)) )
cat("Target Price: ", mean(BootData$Mean)-2*sd(BootData$Mean), " ", mean(BootData$Mean)+2*sd(BootData$Mean))




###Emperical CDF
xbins <-seq(min(coredata(Data_D[,grep(seriesT, colnames(Data_D))])), 
            max(coredata(Data_D[,grep(seriesT, colnames(Data_D))]))+1,1)
ybins <-seq(0, 1, 0.01)

Fn <- ecdf(coredata(Data_D[,grep(seriesT, colnames(Data_D))]))
plot(Fn, axes=F)
axis(side=1, at=xbins)
axis(side=2, at=ybins)
1-Fn(277)
quantile(Data, probs = ybins) 