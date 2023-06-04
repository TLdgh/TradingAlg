FutNewBarSize <- function(DataFile, interval=NULL, barSize=NULL){
  DataInput <-read.csv(file = DataFile, header = T) #get the futures data
  
  if(interval!="daily" & interval!="weekly" & interval!="monthly"){
    barintv<-c(which(format(as.POSIXct(DataInput[,1]), "%H:%M:%S")=="18:00:00"), nrow(DataInput)) #get the interval from 18:00:00 everyday
  }else{
    DataWeekDay<-weekdays(as.Date(DataInput[,1])) #this will be used to convert from daily to weekly
    barintv<-c(which(DataWeekDay=="Monday"), nrow(DataInput)) #get the interval from Monday every week
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



InputCombtxt<-character()
OutputCombtxt<-character()
nam<-character()

for (i in 1:length(FutToBePrepared)){
  if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
    NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4H.csv") #Save the new bar data at this location
    OutputCombtxt<-c(OutputCombtxt,paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HComb.csv"))  #this gives the location of the combined data files
    nam<-c(nam,paste0(FutToBePrepared[[i]][1,"Symb"],"4H"))
  }else{
    NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuous.csv") #Save the new bar data at this location
    OutputCombtxt<-c(OutputCombtxt,paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuousComb.csv"))  #this gives the location of the combined data files
    nam<-c(nam,paste0(FutToBePrepared[[i]][1,"Symb"],"4HContinuous"))
  }
  
  write.csv(FutNewBarSize(DataFile=RawDataLocation[[i]], interval="1H", barSize = 4), file=NewBarOutputfile, row.names = FALSE)
  InputCombtxt<-c(InputCombtxt,NewBarOutputfile)#this gives the input location of data files for candlestick combination app
}    
write.table(InputCombtxt,paste0(getwd(),"/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)


