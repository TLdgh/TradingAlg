#For all securities:
library(tidyverse)
library(tidyquant)
library(plotly)
library(timetk)
library(ggrepel)
library(ggpubr)
library(meboot)
library(KSgeneral)



#######################################################################################################
#######################################################################################################
#For stocks

#SecurityType: FUT for futures, STK for stock, FOREX for forex
#Symb: "AMD", TDOC","CCL","CGC","YALA","BA"or "300454.SS" or "CAD/USD"
#intv: 5F,30F,D,Continuous... for futures; 5min, 30min, daily, weekly... for stocks 
#NumOfMonths: the number of months of the intraday data you want 
#GlobalMarket: US or China, must create a folder with this name 
#OutputSize:  full returns the full-length time series of 20+ years of historical data of the stock, compact has the latest 100 data points
#STEP 1:
av_api_key("<77P2LJBQ51UX69O7>")
SecurityType = c("FOREX")               #SecurityType: STK for stock, FOREX for forex
Symb <-data.frame(Symb=c("CAD/USD"))      #Symb: "AMD", or "CAD/USD"
intv <-c("daily")                     #intv: 1min, 5min, 30min, daily, weekly, monthly for stocks 
NumOfMonths <-c(1)                    #NumOfMonths: the number of months of the intraday data you want. If daily/weekly/monthly, always put 1
GlobalMarket <- c("US")                                             
OutputSize <- "full" 

source("/Users/tengli/R/Script/Candy/PrepStock_Candy.R")


#STEP 2:
#Please combine the data using the CandleStickApp, then proceed with the next step


#STEP 3:
source("/Users/tengli/R/Script/Candy/GetData_Candy.R")
ReadCombData(OutputCombtxt,nam)     #This script load the combined data


#STEP 4:
################Price plotly
source("/Users/tengli/R/Script/StockPlotFunction.R") #everytime we run a function from a different script, we must run this command
StockChart(CADUSD_daily, Title = "CADUSD_daily")

