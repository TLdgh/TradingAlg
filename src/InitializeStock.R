#######################################################################################################
#_____________________________________________INSTRUCTION______________________________________________

#NDuration is the number of durations you want. 
#e.g. if duration="1 Y", then NDuration=44 means the previous 44 years.
#User must choose the barSize (5 mins, 30 mins, 1 day, 1 week) and the duration (1 Y maximum).
#This is specified in IB website: 
#https://interactivebrokers.github.io/tws-api/historical_limitations.html#pacing_violations

#######################################################################################################

#Step 1:---------------------------------------------------------------------------
#specify NDuration, barSize and duration in the following code, then run the chunk:

NDurations<-44 
Sdata<-list()
endDateTime<-format(Sys.time(),"%Y%m%d %H:%M:%S")

for(i in 1:NDurations){
  Contract<-twsEquity(symbol="AAPL")
  SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize="1 day", duration="1 Y", useRTH='1', whatToShow='TRADES') 
  Sdata[[NDurations+1-i]]<-SdataNew
  endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
  Sys.sleep(22)
}  
Sdata<-do.call(rbind, Sdata)

#Step 2:---------------------------------------------------------------------------
#check if the data looks good

head(Sdata)
tail(Sdata)

#Step 3:---------------------------------------------------------------------------
#download the data to the correct directory, make sure to give the correct csv name
#this will write the xts data into a csv, which is a dataframe when later imported

write.zoo(Sdata, sep=",", file=paste0(getwd(),"/Data/OriginalStockData/US/AAPL_daily.csv"))






# Initialize Futures contracts. Please mannually update the barSize and file names. If intraday, please check the format at 00:00:00 midnight.
#barsize<-c("5 mins", "30 mins", "1 hour", "1 day", "1 week")

endtime<-format(with_tz(Sys.time(),tz="Canada/Eastern"),"%Y%m%d %H:%M:%S")
twsNQ <- twsFuture(symbol = "NQ",exch="CME", expiry="20240315", currency="USD", multiplier = "20", include_expired="1")
NQ<- reqHistoricalData(tws, Contract=twsNQ, endDateTime=endtime, barSize="30 mins", duration='4 M', useRTH='0', whatToShow='TRADES')
NQ<-data.frame(Index=format(as.POSIXct(index(NQ),tz="America/Toronto"), "%Y-%m-%d %H:%M:%S"), NQ)
head(NQ,100)
tail(NQ)

write.csv(NQ, file=paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/NQ30F_20240315.csv"), row.names = FALSE)








