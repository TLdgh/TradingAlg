InputCombtxt<-character()
OutputCombtxt<-character()
nam<-character()
countlimit<-0
RawDataLocation<-list()

for (i in 1:length(FutToBePrepared)){
  source("src/MakeName.R") #make a name for your file
  nam<-c(nam,MakeName(FutToBePrepared[[i]][1,]))
  InputFileLoc_Fut<-paste0(getwd(),"/Data/OriginalFuturesData/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #read the original data
  OutputFileLoc_Fut<-paste0(getwd(),"/Data/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #Save the clean data
  CombFileLoc_Fut<-paste0(getwd(),"/CandleStickComb/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], "Comb.csv") #read the combined data
  InputCombtxt <- c(InputCombtxt,OutputFileLoc_Fut) #this gives all the input locations of data files for candlestick combination
  OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Fut)  #this gives all the locations of the combined data files
  
  if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
    source("src/IntradayContract.R")   #Get the original futures intraday data
    Get_IntradayFut(tws,FutToBePrepared[[i]][1,"Symb"],FutToBePrepared[[i]][1,"exch"],FutToBePrepared[[i]][1,"expiry"],FutToBePrepared[[i]][1,"currency"],
                    FutToBePrepared[[i]][1,"multiplier"],FutToBePrepared[[i]][1,"endDateTime"],FutToBePrepared[[i]][1,"barSize"],FutToBePrepared[[i]][1,"duration"],
                    InputFileLoc_Fut)
  }else{
    cat("You are preparing the continuous futures contract!", "\n")
  }
  cat("The following data is: ", nam[i], "\n")
  SecurityType <-FutToBePrepared[[i]][1,"SecurityType"]    #"FUT" #FUT for futures, STK for stock
  source("src/MyStrategy.R")
  DownloadData(nam[i])
  if(i<length(FutToBePrepared)){print("Please wait for 20 seconds");Sys.sleep(20)}
  RawDataLocation[nam[i]]<-InputFileLoc_Fut
}

write.table(InputCombtxt,paste0(getwd(),"/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
