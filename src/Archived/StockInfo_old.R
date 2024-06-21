GetStockInfo<-function(STK,interval,GlobalMarket="US", A_STOK=NULL){
  BasicInfo<-merge(STK,interval)
  StkInfo<-list()
  
  for (i in 1:nrow(BasicInfo)) {
    barsize<-character()
    duration<-character()
    if(BasicInfo[i,2]=="1F"){barsize<-"1 min"; duration<-"1 D"}
    else if(BasicInfo[i,2]=="5F"){barsize<-"5 mins"; duration<-"1 W"}
    else if(BasicInfo[i,2]=="30F"){barsize<-"30 mins"; duration<-"1 M"}
    else if(BasicInfo[i,2]=="daily"){barsize<-"1 day"; duration<-"1 Y"}
    else if(BasicInfo[i,2]=="weekly"){barsize<-"1 week"; duration<-"2 Y"}
    
    if(GlobalMarket=="US"){
      StkInfo[[paste0(BasicInfo[i,1],BasicInfo[i,2])]]<-cbind(SecurityType="STK",Symb=BasicInfo[i,1],intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                              endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), GlobalMarket=GlobalMarket)
    }
    else if (GlobalMarket=="China"){
      StkInfo[[paste0(BasicInfo[i,1],BasicInfo[i,2])]]<-cbind(SecurityType="STK",Symb=BasicInfo[i,1],intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                              endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), GlobalMarket=GlobalMarket, A_STOK=A_STOK)
    }
  }
  return(StkInfo)
}



Get_Stock<-function(tws,Symb,endDateTime,barSize,duration,fileloc){   #this may also get the daily, weekly and monthly data
  #check if data exists already:
  olddata<-try(read.csv(file=fileloc,header = TRUE), stop("No data exists for this stock, please initialize first."))
  toDate<-tail(olddata,1)[,"Index"]
  NMonths<-interval(as.Date(toDate), Sys.time())%/%months(1)+1
  NYears<-interval(as.Date(toDate), Sys.time())%/%years(1)+1
  
  newdata<-list()
  if(barSize=="30 mins"){
    for(i in 1:NMonths){
      Contract<-twsEquity(symbol=Symb)
      SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='1', whatToShow='TRADES') 
      print("Please wait for 20 seconds")
      Sys.sleep(22)
      newdata[[NMonths+1-i]]<-data.frame(Index=as.character(index(SdataNew)),SdataNew,row.names = NULL)
      endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
    }
  }else if(barSize=="1 day"){
    for(i in 1:NYears){
      Contract<-twsEquity(symbol=Symb)
      SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='1', whatToShow='TRADES') 
      print("Please wait for 20 seconds")
      Sys.sleep(22)
      newdata[[NYears+1-i]]<-data.frame(Index=as.character(index(SdataNew)),SdataNew,row.names = NULL)
      endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
    }
  }
  newdata<-do.call(rbind, newdata)
  
  #Detect if stock split is announced:
  pivotdate<-newdata$Index[1]
  fac<-sort(table(as.numeric(newdata[which(newdata$Index==pivotdate),2:5] / olddata[which(olddata$Index==pivotdate),2:5])), 
            decreasing = TRUE)[1]%>%names()%>%as.numeric()
  if(fac!=1){
    if(readline(prompt = "Need to split the stock. Please confirm: Y/N")=="Y"){
      olddata[,2:5]<-fac*olddata[,2:5]}else{stop("Execution stopped.")}
  }else{
    cat("No need to split the stock.", "\n")
  }
  
  newdata<-union(olddata[which(olddata$Index<newdata$Index[1]),],newdata)
  write.csv(newdata, file=fileloc, row.names = FALSE) #this will write the xts data into a csv, which is a dataframe when later imported
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
