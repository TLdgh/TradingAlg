GetFutInfo<-function(FUT,interval){
  BasicInfo<-merge(FUT,interval)
  FutInfo<-list()
  
  for (i in 1:nrow(BasicInfo)) {
    barsize<-character()
    duration<-character()
    if(BasicInfo[i,2]=="30S"){barsize<-"30 secs";duration<-"28800 S"}
    else if(BasicInfo[i,2]=="1F"){barsize<-"1 min"; duration<-"2 D"}
    else if(BasicInfo[i,2]=="5F"){barsize<-"5 mins"; duration<-"2 W"}
    else if(BasicInfo[i,2]=="30F"){barsize<-"30 mins"; duration<-"1 M"}
    else if(BasicInfo[i,2]=="1H"){barsize<-"1 hour"; duration<-"3 M"}
    else{barsize<-"Continuous"; duration<-"Continuous"}
    
    if(BasicInfo[i,1]=="NQ"){
      FutInfo[[paste0("NQ",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("NQ"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                   endDateTime=format(with_tz(Sys.time(),tz="Canada/Eastern"),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20231215",currency="USD",multiplier="20")}
    else if(BasicInfo[i,1]=="GC"){
      FutInfo[[paste0("GC",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("GC"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                   endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="COMEX",expiry="20211229",currency="USD",multiplier="100")}
    else if(BasicInfo[i,1]=="CAD"){
      FutInfo[[paste0("CAD",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("CAD"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                    endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20220315",currency="USD",multiplier="100000")}
  }
  return(FutInfo)
}


FutNewBarSize <- function(DataFile, interval=NULL, barSize=NULL){
  DataInput <-read.csv(file = DataFile, header = T) #get the futures data
  
  if(interval %in% c("1H","1HContinuous")){ #currently only support conversion from 1H to 4H
    barintv<-c(which(format(as.POSIXct(DataInput[,1]), "%H:%M:%S")=="18:00:00"), nrow(DataInput)) #get the interval from 18:00:00 everyday
  }
  
  NewData <-data.frame()
  for(n in 1:(length(barintv)-1)){
    kmod<-(barintv[n+1]-barintv[n])%%barSize #the remainder of bars after you convert to get the new bar
    k<-barintv[n+1]-barintv[n]-kmod #the number of bars you need to convert for the new bar size. e.g. hourly data convert to 4h
    l<-1
    i<-1    
    
    temp <- data.frame()
    NewTemp <- DataInput[barintv[n]:(barintv[n+1]-1),] #the data within the interval
    
    while (l <= k/barSize) {     #combine every n bars to get the new bar
      temp[l,1] <- NewTemp[i,1] #this is the Date 
      temp[l,2] <- NewTemp[i,2] #Open
      temp[l,3] <- max(NewTemp[i:(i+barSize-1),3])   #High
      temp[l,4] <- min(NewTemp[i:(i+barSize-1),4]) #Low
      temp[l,5] <- NewTemp[i+barSize-1,5] #Close
      temp[l,6] <- sum(NewTemp[i:(i+barSize-1),6]) #sum of volumes
      l=l+1
      i=i+barSize
    }
    if(kmod!=0){  # combining the remainder of bars to get the new bar
      temp[l,1] <- NewTemp[i,1]
      temp[l,2] <- NewTemp[i,2]
      temp[l,3] <- max(NewTemp[i:(i+kmod-1),3])
      temp[l,4] <- min(NewTemp[i:(i+kmod-1),4])
      temp[l,5] <- NewTemp[i+kmod-1,5]
      temp[l,6] <- sum(NewTemp[i:(i+kmod-1),6])
    }
    NewData <-rbind(NewData,temp) 
  }
  
  colnames(NewData)<-c("Date", "Open", "High", "Low", "Close", "Volume")
  NewData <- NewData[order(NewData$Date, decreasing = TRUE),] #make sure the data is in decreasing order fore candlestick combination
  return(NewData)
}

