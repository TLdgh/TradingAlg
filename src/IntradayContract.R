#to find the contract details like conId, go to IB Watchlist, right click contract name and click Financial Instrument Info and select Detail
#see duration and barsize in https://interactivebrokers.github.io/tws-api/historical_limitations.html#pacing_violations


Get_IntradayFut<-function(tws,Symb,exch,expiry,currency,multiplier,endDateTime,barSize,duration,fileloc){
  Contract<-twsFuture(symbol=Symb,exch=exch, expiry=expiry, currency=currency, multiplier=multiplier, include_expired="1")
  Fdata<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='0', whatToShow='TRADES') 
  write.zoo(Fdata, sep=",", file=fileloc) #this will write the xts data into a csv, which is a dataframe when later imported
}


Get_Stock<-function(tws,Symb,endDateTime,barSize,duration,fileloc){   #this may also get the daily, weekly and monthly data
  #check if data exists already:
  currentdata<-try(read.csv(file=fileloc,header = TRUE), stop("No data exists for this stock, please initialize first."))
  toDate<-tail(currentdata,1)[,"Index"]
  NMonths<-interval(as.Date(toDate), Sys.time())%/%months(1)+1
  
  Sdata<-list()
  for(i in 1:NMonths){
    Contract<-twsEquity(symbol=Symb)
    SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='1', whatToShow='TRADES') 
    Sdata[[NMonths+1-i]]<-data.frame(Index=as.character(index(SdataNew)),SdataNew,row.names = NULL)
    endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
    if(NMonths>1){Sys.sleep(22)}
  }
  
  Sdata<-do.call(rbind, Sdata)
  Sdata<-union(currentdata,Sdata)
  write.csv(Sdata, file=fileloc, row.names = FALSE) #this will write the xts data into a csv, which is a dataframe when later imported
}

Get_ChineseStock<-function(Symb,freq,fileloc){
  if (freq=="daily"){
    TushareAPI <- pro_bar(token="e75513f388ed87cb80c5a75cc2e4f6f8a0e9411073249a8793aee47e") #for Chinese A shares. need package "Tushare".
    Sdata <- TushareAPI(ts_code=Symb, start_date = "20000101", adj = "qfq")
  }else{
    TushareAPI <- pro_api(token="e75513f388ed87cb80c5a75cc2e4f6f8a0e9411073249a8793aee47e") #for Chinese A shares. need package "Tushare".
    Sdata <- TushareAPI(ts_code=Symb, start_date = "20000101", adj = "qfq", api_name = freq)}
  
  Sdata<-na.omit(Sdata)
  Sdata<-Sdata%>%mutate(trade_date = as.character(gsub('^(\\d{4})(\\d{2})(\\d{2})$', '\\1-\\2-\\3', trade_date)), amount=as.numeric(amount))
  Sdata<-Sdata[,c("trade_date","open","high","low","close","amount")]
  colnames(Sdata) <- c("Date", "Open", "High","Low", "Close", "Volume")
  Sdata$Date <- as.character(Sdata$Date)
  write.csv(Sdata,file = fileloc, row.names = FALSE)
}
