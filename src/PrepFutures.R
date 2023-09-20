InputCombtxt<-character()
OutputCombtxt<-character()
nam<-character()
countlimit<-0
RawDataLocation<-list()

for (i in 1:length(FutToBePrepared)){
  source("/Users/tengli/R/TradingAlg/src/MakeName.R") #make a name for your file
  nam<-c(nam,MakeName(FutToBePrepared[[i]][1,]))
  InputFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/Data/OriginalFuturesData/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #read the original data
  OutputFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/Data/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #Save the clean data
  CombFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], "Comb.csv") #read the combined data
  InputCombtxt <- c(InputCombtxt,OutputFileLoc_Fut) #this gives all the input locations of data files for candlestick combination
  OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Fut)  #this gives all the locations of the combined data files
  
  if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
    Get_IntradayFut(tws,FutToBePrepared[[i]][1,"Symb"],FutToBePrepared[[i]][1,"exch"],FutToBePrepared[[i]][1,"expiry"],FutToBePrepared[[i]][1,"currency"],
                    FutToBePrepared[[i]][1,"multiplier"],FutToBePrepared[[i]][1,"endDateTime"],FutToBePrepared[[i]][1,"barSize"],FutToBePrepared[[i]][1,"duration"],
                    InputFileLoc_Fut)
  }else{
    cat("You are preparing the continuous futures contract!", "\n")
    if(FutToBePrepared[[i]][1,"intv"]=="5FContinuous"){Get_ContinuousFut(NQ5FExpD)}
    else if(FutToBePrepared[[i]][1,"intv"]=="30FContinuous"){Get_ContinuousFut(NQ30FExpD)}
    else if(FutToBePrepared[[i]][1,"intv"]=="1HContinuous"){Get_ContinuousFut(NQ1HExpD)}
    else if(FutToBePrepared[[i]][1,"intv"]=="Continuous"){Get_ContinuousFut(NQExpD)}
    else if(FutToBePrepared[[i]][1,"intv"]=="WContinuous"){Get_ContinuousFut(NQWExpD)}
  }
  cat("The following data is: ", nam[i], "\n")
  SecurityType <-FutToBePrepared[[i]][1,"SecurityType"]    #"FUT" #FUT for futures, STK for stock
  source("/Users/tengli/R/TradingAlg/src/MyStrategy.R")
  DownloadData(nam[i])
  if(i<length(FutToBePrepared)){print("Please wait for 20 seconds");Sys.sleep(20)}
  RawDataLocation[nam[i]]<-InputFileLoc_Fut
}

#check if there's 1H data:
Exist1H<-which(names(RawDataLocation) %in% c("NQ1H","NQ1HContinuous"))
if(length(Exist1H)!=0){
  for(i in Exist1H){
    if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
      NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4H.csv") #Save the new bar data at this location
      OutputCombtxt[i]<-paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HComb.csv")  #this gives the location of the combined data files
      nam[i]<-paste0(FutToBePrepared[[i]][1,"Symb"],"4H")
    }else{
      NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuous.csv") #Save the new bar data at this location
      OutputCombtxt[i]<-paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuousComb.csv")  #this gives the location of the combined data files
      nam[i]<-paste0(FutToBePrepared[[i]][1,"Symb"],"4HContinuous")
    }
    RawDataLocation[nam[i]]<-paste0("/Users/tengli/R/TradingAlg","/Data/OriginalFuturesData/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv")
    write.csv(FutNewBarSize(DataFile=RawDataLocation[[i]], interval="1H", barSize = 4), file=NewBarOutputfile, row.names = FALSE)
    InputCombtxt[i]<-NewBarOutputfile#this gives the input location of data files for candlestick combination app
  }
}

write.table(InputCombtxt,paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
