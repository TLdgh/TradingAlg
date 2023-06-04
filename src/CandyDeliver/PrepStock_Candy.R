StkToBePrepared <- merge(x=Symb,y=data.frame(SecurityType=SecurityType,
                                             list(intv=intv,NumOfMonths=NumOfMonths), 
                                             GlobalMarket=GlobalMarket,
                                             OutputSize=OutputSize), all = TRUE)

InputCombtxt<-character()
OutputCombtxt<-character()
nam<-character()
countlimit<-0
RawDataLocation<-list()

for (i in 1:nrow(StkToBePrepared)) {
  source("C:\\R\\Script\\MakeName_Candy.R") #make a name for your file
  nam<-c(nam,MakeName(StkToBePrepared[i,]))
  
  #Please update your file locations here if necessary:
  InputFileLoc_Stk<-paste("C:\\R\\Data\\", StkToBePrepared[i,"GlobalMarket"], "\\", nam[i], ".csv", sep = "") #save the clean data
  CombFileLoc_Stk<-paste("C:\\CandleStickComb\\",StkToBePrepared[i,"GlobalMarket"], "\\", nam[i], "Comb.csv", sep = "") #read the combined data
  InputCombtxt <- c(InputCombtxt,InputFileLoc_Stk) #this gives all the input locations of data files for candlestick combination
  OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Stk)  #this gives all the locations of the combined data files
  
  #Prepare the data for plot
  SecurityType <-StkToBePrepared[i,"SecurityType"]    #"FUT" #FUT for futures, STK for stock
  Symb<-StkToBePrepared[i,"Symb"] 
  intv<-StkToBePrepared[i,"intv"]
  NumOfMonths<-StkToBePrepared[i,"NumOfMonths"]
  GlobalMarket<-StkToBePrepared[i,"GlobalMarket"]
  OutputSize<-StkToBePrepared[i,"OutputSize"]
  cat("The following data is: ", nam[i], "\n")
  source("C:\\R\\Script\\GetData_Candy.R")
  countlimit<-DownloadData()
  RawDataLocation[nam[i]]<-InputFileLoc_Stk
  
}

write.table(InputCombtxt,"C:\\CandleStickComb\\InputLoc.txt",sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
