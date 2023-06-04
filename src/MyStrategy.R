DownloadData<- function(nam){
  if (SecurityType=="FUT") {
    Fut_data<-read.csv(file = InputFileLoc_Fut, header = T)
    Fut_data<-Fut_data[,1:6]
    colnames(Fut_data) <- c("Date", "Open", "High","Low", "Close", "Volume")
    Fut_data <- Fut_data[order(Fut_data$Date, decreasing = TRUE),]
    print(head(Fut_data))
    print(tail(Fut_data))
    
    write.csv(Fut_data, file = OutputFileLoc_Fut, row.names = FALSE)
  }
  else if(SecurityType == "STK" & GlobalMarket=="US"){
    Stk_data<-read.csv(file = InputFileLoc_Stk, header = T)
    Stk_data<-Stk_data[,1:6]
    colnames(Stk_data) <- c("Date", "Open", "High","Low", "Close", "Volume")
    Stk_data <- Stk_data[order(Stk_data$Date, decreasing = TRUE),]
    print(head(Stk_data))
    print(tail(Stk_data))
    
    write.csv(Stk_data, file = OutputFileLoc_Stk, row.names = FALSE)
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
    
    #source("/Users/tengli/R/Script/Proceed.R") #check if data is ready ? if yes then we can proceed with candlestick combination   
    #if (Proceed()=="n") {stop("please recheck data")}else{print("Data is saved! You may combine the condlesticks!")}
    write.csv(fx_DWM, file =InputFileLoc_Stk, row.names = FALSE)
    countlimit<-countlimit+1
    if(countlimit%%5==0){print("Please wait for 1 minute.");Sys.sleep(60);countlimit<-0}
    
  }
  else if(SecurityType == "STK" & GlobalMarket=="China"){
    Stk_data<-read.csv(file = InputFileLoc_Stk, header = T)
    Stk_data<-Stk_data[,1:6]
    colnames(Stk_data) <- c("Date", "Open", "High","Low", "Close", "Volume")
    Stk_data <- Stk_data[order(Stk_data$Date, decreasing = TRUE),]
    print(head(Stk_data))
    print(tail(Stk_data))
    
    write.csv(Stk_data, file = OutputFileLoc_Stk, row.names = FALSE)
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

