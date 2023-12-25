InputCombtxt<-character()
OutputCombtxt<-character()
nam<-character()
countlimit<-0
RawDataLocation<-list()

for (i in 1:length(StkToBePrepared)) {
  source("src/MakeName.R") #make a name for your file
  nam<-c(nam,MakeName(StkToBePrepared[[i]][1,]))
  InputFileLoc_Stk<-paste0(getwd(),"/Data/OriginalStockData/", StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], ".csv") #save the clean data
  OutputFileLoc_Stk<-paste0(getwd(),"/Data/", StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], ".csv") #Save the clean data
  CombFileLoc_Stk<-paste0(getwd(),"/CandleStickComb/",StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], "Comb.csv") #read the combined data
  InputCombtxt <- c(InputCombtxt,OutputFileLoc_Stk) #this gives all the input locations of data files for candlestick combination
  OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Stk)  #this gives all the locations of the combined data files
  
  #Prepare the data for plot
  if(StkToBePrepared[[i]][1,"GlobalMarket"]=="US"){
    Get_Stock(tws,StkToBePrepared[[i]][1,"Symb"],StkToBePrepared[[i]][1,"endDateTime"],StkToBePrepared[[i]][1,"barSize"],StkToBePrepared[[i]][1,"duration"],
              InputFileLoc_Stk)
  }
  else{
    Get_ChineseStock(Symb=StkToBePrepared[[i]][1,"Symb"][[1]], freq=StkToBePrepared[[i]][1,"intv"][[1]], fileloc=InputFileLoc_Stk)
  }
  
  cat("The following data is: ", nam[i], "\n")
  SecurityType <-StkToBePrepared[[i]][1,"SecurityType"]
  GlobalMarket<-StkToBePrepared[[i]][1,"GlobalMarket"]
  source("src/MyStrategy.R")
  countlimit<-DownloadData(nam[i])
  RawDataLocation[nam[i]]<-InputFileLoc_Stk
}

write.table(InputCombtxt,paste0(getwd(),"/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
