library(tidyverse)
library(tidyquant)
library(plotly)


#####Bootstrap
SampleData<-c(coredata(BA_daily$Open),coredata(BA_daily$High),coredata(BA_daily$Low),coredata(BA_daily$Close) )
nboot <- 1000
data <- matrix(sample(SampleData, size = length(SampleData)*nboot, replace = T), nrow = nboot)#draw data with the same length of the sample for nboot times
BootData <- data.frame(Prob = rep(0,1,nboot), Mean = rep(0,1,nboot))#create initial boot data
for (i in 1:nboot) {
  Fn <- ecdf(data[i,])
  BootData[i,1] <- 1-Fn(100) #what's the probability of p > currentprice
  BootData[i,2] <- mean(data[i,]) #what's the mean of the ith bootrun
  
}
mean(BootData$Prob)
sd(BootData$Prob)
hist(BootData$Prob)
mean(BootData$Mean)
sd(BootData$Mean)

cat("Estimated probability: ", sum(Indicators)*(mean(BootData$Prob)+2*sd(BootData$Prob)) )
cat("Target Price: ", mean(BootData$Mean)-1.96*sd(BootData$Mean)/sqrt(length(BootData$Mean)), " ", mean(BootData$Mean)+1.96*sd(BootData$Mean)/sqrt(length(BootData$Mean)))




###Emperical CDF
xbins <-seq(min(coredata(BA_daily$Close)), 
            max(coredata(BA_daily$Close))+1,1)
ybins <-seq(0, 1, 0.01)

Fn <- ecdf(coredata(BA_daily$Close))
plot(Fn, axes=F)
axis(side=1, at=xbins)
axis(side=2, at=ybins)
1-Fn(100)
quantile(Data, probs = ybins) 