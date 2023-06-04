Contract<-twsFuture(symbol="NQ",exch="GLOBEX", expiry="20211217",currency="USD", multiplier="20", include_expired="1")

snapShotTest<-function(twsCon, eWrapper, timestamp, file, playback=1, ...){
  if(missing(eWrapper)){eWrapper <- eWrapper()}
  
  names(eWrapper$.Data$data)<-eWrapper$.Data$symbols 
  eWrapper$errorMessage<-eWNULL$errorMessage
  
  con <- twsCon[[1]]
  if(inherits(twsCon, 'twsPlayback')) {
    sys.time <- NULL
    while(TRUE) {
      if(!is.null(timestamp)){
        # MktData
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, character(),2),collapse=' '), timestamp))
        if(!is.null(last.time)){
          Sys.sleep((sys.time-last.time)*playback)
        }
        curMsg <- .Internal(readBin(con, "character", 1L,NA_integer_,TRUE, FALSE)) 
        if(length(curMsg) < 1){next}
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      } else{
        # RealTimeBars
        curMsg<-readBin(con, character(), 1)
        if(length(curMsg) < 1){next}
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        if(curMsg == .twsIncomingMSG$REAL_TIME_BARS){Sys.sleep(5 * playback)}
      }
    }
  } 
  else { 
    while(isConnected(twsCon)) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- .Internal(readBin(con, "character", 1L,NA_integer_,TRUE, FALSE)) 
      if(!is.null(timestamp)){
        processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, twsCon, ...)
      } else{
        processMsg(curMsg, con, eWrapper, timestamp, file, twsCon, ...)
      }
      if(!any(sapply(eWrapper$.Data$data, is.na))){
        return(do.call(rbind,lapply(eWrapper$.Data$data,as.data.frame)))
      } 
    }
  }
}

f<-file("TestDataReqRealTimeBar.csv", open = "w")
eWNULL <-eWrapper(debug = NULL, errfile = f)
eW<-eWrapper.data(1)
eW$errorMessage <-eWNULL$errorMessage

Fdata2<-reqMktData(tws, Contract=Contract,eventWrapper=eW,CALLBACK = snapShotTest, file = f)

print(Fdata2)













