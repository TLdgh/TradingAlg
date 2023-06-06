GetData_Candy<- function(nam){
  if(SecurityType == "STK" & GlobalMarket=="US"){
    if(intv=="daily" | intv=="weekly" | intv=="monthly"){
      
      if(intv=="daily"){
        stk_DWM <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_DAILY_ADJUSTED", outputsize=OutputSize)
        stk_DWM <- as.data.frame(subset(stk_DWM,select=c(timestamp,open,high,low,close,volume)))
      }
      else if(intv=="weekly"){
        stk_DWM <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_WEEKLY_ADJUSTED", outputsize=OutputSize)
        stk_DWM$open<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$open
        stk_DWM$high<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$high
        stk_DWM$low<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$low
        stk_DWM <- as.data.frame(subset(stk_DWM,select=c(timestamp,open,high,low,adjusted_close,volume)))
      }
      else if(intv=="monthly"){
        stk_DWM <- tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_MONTHLY_ADJUSTED", outputsize=OutputSize)
        stk_DWM$open<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$open
        stk_DWM$high<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$high
        stk_DWM$low<-stk_DWM$adjusted_close/stk_DWM$close*stk_DWM$low
        stk_DWM <- as.data.frame(subset(stk_DWM,select=c(timestamp,open,high,low,adjusted_close,volume)))
      }
      
      colnames(stk_DWM) <- c("Date", "Open", "High","Low", "Close", "Volume")
      stk_DWM<-stk_DWM[order(stk_DWM$Date, decreasing = TRUE),]
      print(head(stk_DWM))
      print(tail(stk_DWM))
      
      write.csv(stk_DWM, file =InputFileLoc_Stk, row.names = F)
      countlimit<-countlimit+1
      if(countlimit%%5==0){print("Please wait for 1 minute.");Sys.sleep(60);countlimit<-0}
    }
    else{
      sliceNum<-vector()
      stock_intraday<-list()
      for (i in 1:NumOfMonths) {
        sliceNum<-paste("year1month",i,sep="")
        stock_intraday[[sliceNum]]<-tq_get(Symb, get="alphavantage", av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=intv, slice=sliceNum)
        countlimit<-countlimit+1
        if(countlimit%%5==0){print("Please wait for 1 minute.");Sys.sleep(60);countlimit<-0}
      }
      
      stock_intraday<-data.frame(do.call(rbind,stock_intraday),row.names = NULL)
      stock_intraday$HMS <- format(stock_intraday$time, "%H:%M:%S") #extract only the trading hours
      stock_intraday <- subset(stock_intraday,stock_intraday$HMS>="09:35:00" & stock_intraday$HMS<= "16:00:00")#extract only the trading hours
      stock_intraday[,c("symbol","HMS")]<-list(NULL)
      colnames(stock_intraday) <- c("Date", "Open", "High","Low", "Close", "Volume")
      stock_intraday<-stock_intraday[order(stock_intraday$Date, decreasing = TRUE),]
      print(head(stock_intraday))
      print(tail(stock_intraday))
      
      write.csv(stock_intraday, file = InputFileLoc_Stk, row.names = F)
    }
  }
  else if(SecurityType == "FOREX"){
    if(intv=="daily"){fx_DWM <- tq_get(Symb, get="alphavantage", av_fun="FX_DAILY", outputsize=OutputSize)
    }else if(intv=="weekly"){
      fx_DWM <- tq_get(Symb, get="alphavantage", av_fun="FX_WEEKLY", outputsize=OutputSize)
    }else if(intv=="monthly"){
      fx_DWM <- tq_get(Symb, get="alphavantage", av_fun="FX_MONTHLY", outputsize=OutputSize)
    }else{
      fx_DWM <- tq_get(Symb, get="alphavantage", av_fun="FX_INTRADAY", interval=intv, outputsize=OutputSize)
    }
    
    fx_DWM <- cbind(as.data.frame(subset(fx_DWM,select=-c(symbol))), Volume=c(0))
    colnames(fx_DWM) <- c("Date", "Open", "High","Low", "Close","Volume")
    fx_DWM<-fx_DWM[order(fx_DWM$Date, decreasing = TRUE),]
    print(head(fx_DWM))
    print(tail(fx_DWM))
    
    write.csv(fx_DWM, file =InputFileLoc_Stk, row.names = F)
    countlimit<-countlimit+1
    if(countlimit%%5==0){print("Please wait for 1 minute.");Sys.sleep(60);countlimit<-0}
  }
  return(countlimit)
}


ReadCombData<-function(OutputCombtxt,nam){  #This script read and load the combined data
  for (i in 1:length(OutputCombtxt)) { 
    CombData <- read.csv(OutputCombtxt[i], header = T) 
    CombData <- CombData[order(CombData$Date, decreasing = FALSE),]
    cat("Combined data is imported, please check the combined data!", "\n", "The following data is: ",nam[i], "\n")
    print(head(CombData))
    print(tail(CombData))
    assign(nam[i], CombData, envir = .GlobalEnv)
  }
}

