ChartReplay<-function(Pricedata, Title, PausePeriod=3, StartCandle=NULL,StartDate=NULL, UerInput="N"){
  #determine from which candlestick to start
  if(is.null(StartCandle)==TRUE & is.null(StartDate)==TRUE){i<-1}
  else if(is.null(StartCandle)==FALSE){i<-StartCandle}
  else{i<-which(Pricedata$Date==StartDate)}
    
    while(i<=nrow(Pricedata)){
      possibleError<-tryCatch( #This returns an object depending on error or not
        #The try part:
        expr = {
          #Check if there's an error, if not proceed, else goes to error function
          res<-StockChart(Pricedata[1:i,], Title)
          
          #evaluate based on UserInput or not
          if(UerInput=="Y"){
            proceed<-readline(prompt="Next bar? Y/N")
            if(proceed=="Y"){print(res);i<-i+1;next}else{break}
          }else{print(paste0("Chart is ready! Candlestick: ",i));print(res);i<-i+1;Sys.sleep(PausePeriod)}
        },
        
        #The error handling part
        error = function(e){
          print(paste0("Chart is not ready for the ",i, "th candlestick. Please wait..."))
          e #this is needed because it is the original error message, and it should be returned to possibleError variable
        }
      )
      if(inherits(possibleError,"error")){i<-i+1}
    }
}

