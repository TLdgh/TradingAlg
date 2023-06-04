library(tidyverse)
library(tidyquant)
library(plotly)
library(TTR)

av_api_key("<77P2LJBQ51UX69O7>")
Symb <- "pltr"
intv<- "30min"
seriesT <- "close"
PeriodStart_I<- as.POSIXct("2021-07-01 10:30:00", tz="UTC")
PeriodEnd_I<- as.POSIXct("2021-07-08 15:00:00", tz="UTC")

####Data must have recent ones on the top rows, i.e. time should be descending.
####create data first. For combining the candles, go to Terminal, change directory cd CandleStick/CandleStickCombination/CandleStickCombination 
####then enter csc Program.cs to get Program.exe, then enter mono Program.exe and enter the file path /Users/tengli/R/Data/xxxx.csv
####the combined data will be saved in CandleStcikComb as output.csv. Rename it.

stock_intraday1 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month1")
stock_intraday2 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month2")
stock_intraday3 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month3")
stock_intraday4 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month4")
stock_intraday5 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month5")
stock_intraday6 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month6")
stock_intraday7 <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice="year1month7")

stock_intraday <- rbind(stock_intraday1, stock_intraday2,stock_intraday3)
stock_intraday$HMS <- format(stock_intraday$time, "%H:%M:%S") #extract only the trading hours
stock_intraday <- subset(stock_intraday,stock_intraday$HMS>="09:35:00" & stock_intraday$HMS<= "16:00:00")#extract only the trading hours
Data_I <- as.data.frame(stock_intraday[,-c(1,8)])
colnames(Data_I) <- c("Date", "Open", "High","Low", "Close", "Volume")
head(Data_I)
tail(Data_I)

#Data_I <- subset(Data_I,Data_I$time>=PeriodStart_I & Data_I$time<= PeriodEnd_I) #subset only the period we want

nam <- paste(toupper(Symb),"30F", sep = "")
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




#############################################################################
###Price chartSeries
as.xts(TDOC30F[,-1],order.by = TDOC30F$Date)%>%Cl()%>%chartSeries(TA="addVo();addRSI();addMACD();addSMA(n=10);addSMA(n=20);addSMA(n=60);addSMA(n=200)")


###Price ggplot
pricebreaks <-seq(min(TDOC30F[,grep(seriesT, colnames(TDOC30F))]), 
                  max(TDOC30F[,grep(seriesT, colnames(TDOC30F))]),5)
timelabels <- seq.POSIXt(from =min(TDOC30F$timestamp), to=max(TDOC30F$timestamp), by="5 mins")%>%
  format(format = "%b-%e %Hh")

ggplot(Data_I, aes(x=index(Data_I), y=close)) + 
  geom_line(aes(color = "Price")) +
  scale_x_continuous(breaks=seq(1,nrow(Data_I), length.out = length(timelabels)), labels=timelabels) +
  scale_y_continuous(breaks=pricebreaks)+
  theme(axis.text.x = element_text(face = "plain", color = "black",angle = 45),
        axis.text.y = element_text(face = "bold", size = 12))+
  geom_line(aes(y=MA10, color="MA10"))+
  geom_line(aes(y=MA20, color="MA20"))+
  geom_line(aes(y=MA60, color="MA60"))+
  geom_line(aes(y=MA200, color="MA200"))+
  ggtitle("Closed Price") + 
  xlab("Date") + ylab("Price")+
  scale_colour_manual("Series", values=c("Price"="gray40", "MA10"="orange", "MA20"="darkcyan", 
                                         "MA60"="firebrick4", "MA200"="darkblue"))


###MACD plot
ggplot(yala_macd)+ 
  geom_line(aes(x=Date,y=DIFF),color="orange")+
  geom_line(aes(x=Date, y=DEA), color = "blue")+
  geom_col(aes(x=Date, y= MACD), color="grey")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))
#Or
ggplot(MACD_Intraday, aes(x =index(MACD_Intraday))) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = MACD_Intraday$macd, color = "MACD")) +
  geom_line(aes(y = MACD_Intraday$macd_signal, color = "Signal")) +
  geom_bar(aes(y = MACD_Intraday$macd_hist), stat = "identity", color = palette_light()[[1]])+
  scale_colour_manual("Series", values=c("MACD"="orange", "Signal"="blue"))


###RSI plot
ggplot(RSI_Intraday, aes(x =index(RSI_Intraday))) + 
  geom_hline(yintercept = 70, color = palette_light()[[1]]) +  #scales::show_col(palette_light())
  geom_hline(yintercept = 30, color = palette_light()[[1]]) +
  geom_line(aes(y=rsi, color = "rsi")) 


### Indicators
Indicators <- matrix(0, ncol = 11)
colnames(Indicators)<- c("Ind_MA10", "Ind_MA20", "Ind_MA60", "Ind_MA200", "Ind_RSI", "Ind_MACD", 
                         "Resistence", "Support", "VolScale","AllTimeHigh","CandleMeasure")
Resistence <-180
Support <-165.08
VolScale <- last(Data_I$volume)[[1]]/mean(Data_I$volume)
AllTimeHigh<- 3
CandleMeasure <- 7

Indicators[1,1] <- if (SMA10_I[1,3]<=coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))) 1 else -1
Indicators[1,2] <- if (SMA20_I[1,3]<=coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))) 1 else -1
Indicators[1,3] <- if (SMA60_I[1,3]<=coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))) 1 else -1
Indicators[1,4] <- if (SMA200_I[1,3]<=coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))) 1 else -1
Indicators[1,7] <- if (coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))>=Resistence) 1 else 0
Indicators[1,8] <- if (coredata(last(Data_I[,grep(seriesT, colnames(Data_I))]))<=Support) -1 else 0
Indicators[1,9] <- VolScale
Indicators[1,10] <-AllTimeHigh
Indicators[1,11] <-CandleMeasure

TargetDate <-last(Data_I$timestamp)

if(RSI_Intraday[RSI_Intraday$Date==TargetDate,]$rsi <30){
  Indicators[1,5]=1} else if(RSI_Intraday[RSI_Intraday$Date==TargetDate,]$rsi >70){
    Indicators[1,5]=-1} else {Indicators[1,5]=0}

Ind_MACD_Matrix <- matrix(0, ncol=3)
Ind_MACD_Matrix[1,1]<-if(MACD_Intraday[MACD_Intraday$Date==TargetDate,]$macd > MACD_Intraday[MACD_Intraday$Date==TargetDate,]$macd_signal) 1 else -1
Ind_MACD_Matrix[1,2]<-if(MACD_Intraday[MACD_Intraday$Date==TargetDate,]$macd > 0) 1 else -1 
Ind_MACD_Matrix[1,3]<-if(MACD_Intraday[MACD_Intraday$Date==TargetDate,]$macd_signal>0) 1 else -1
Indicators[1,6] <- sum(Ind_MACD_Matrix)

print(Indicators)
Indicator_Weight <- length(which(Indicators>=0))/length(which(Indicators<0))
cat("The total indicator is: ", Indicator_Weight)


#####Bootstrap
SampleData<-c(coredata(stock_intraday$open),coredata(stock_intraday$high),coredata(stock_intraday$low),coredata(stock_intraday$close) )
nboot <- 1000
data <- matrix(sample(SampleData, size = length(SampleData)*nboot, replace = T), nrow = nboot)
BootData <- data.frame(Prob = rep(0,1,nboot), Mean = rep(0,1,nboot))
for (i in 1:nboot) {
  Fn <- ecdf(data[i,])
  BootData[i,1] <- 1-Fn(currentprice)
  BootData[i,2] <- mean(data[i,])
}

MeanProb<-mean(BootData$Prob)
SdProb<-sd(BootData$Prob)
#hist(BootData$Prob)
MeanPrice<-mean(BootData$Mean)
SdPrice<-sd(BootData$Mean)

cat("Estimated probability:","\n",c(Indicator_Weight*(MeanProb-2*SdProb),Indicator_Weight*(MeanProb+2*SdProb)),"\n")
cat("Target Price:","\n", c(MeanPrice-2*SdPrice, MeanPrice+2*SdPrice))








###Return
InitialP <-260
FinalP <- 255
Nshare <-70
PL <- (Nshare*(FinalP-InitialP)-9.95*2)/(Nshare*InitialP+9.95*2)
cat("Profit/Loss rate is: ", PL, "\n")
cat("Price at 5% loss:", (-0.05*(Nshare*InitialP+9.95*2)+9.95*2)/Nshare+InitialP, "\n" )






###Emperical CDF
xbins <-seq(min(coredata(Data_I$close)), max(coredata(Data_I$close)+0.01,0.01))
ybins <-seq(0, 1, 0.01)

Fn <- ecdf(coredata(Data_I$close))
plot(Fn, axes=F)
axis(side=1, at=xbins)
axis(side=2, at=ybins)
1-Fn(277)
quantile(Data, probs = ybins)  



##### fit models
fitN <- fitdist(BootData$Prob, distr = "norm", method = "mle")
plot(fitN)
fitEXP <- fitdist(y$Data, distr = "exp", method = "mle")
plot(fitEXP)
fitG <- fitdist(y$Data, distr = "gamma", method = "mle")
plot(fitG)
fitW <- fitdist(y$Data, distr = "weibull", method = "mle")
plot(fitW)
fitLN <- fitdist(y$Open, distr = "lnorm")   #This is the best one
plot(fitLN)
denscomp(fitLN, main = "LogNormal")
cdfcomp(fitLN, main = "CDF LogNormal")
