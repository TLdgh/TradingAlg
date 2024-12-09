GetStkInfo<-setRefClass(
  "StkToBePrepared",
  fields = list(StkToBePrepared="list", STK="character", interval="character",
                InputCombtxt="character",OutputCombtxt="character",RawDataLocation="list",nam="character",
                tws="environment", RealData="logical", TimeZ="character", GlobalMarket="character"),
  methods = list(
    initialize=function(STK,interval,tws,RealData = FALSE,GlobalMarket="US"){
      #Create the StkToBePrepared attribute
      BasicInfo<-merge(STK,interval)
      .self$TimeZ<-"America/Toronto"
      .self$StkToBePrepared<-.self$populateStkInfo(BasicInfo,GlobalMarket)
      
      #Prepare Stock information
      .self$InputCombtxt<-character()
      .self$OutputCombtxt<-character()
      .self$RawDataLocation<-list()
      .self$nam<-character()
      .self$PrepStock(tws, RealData)
    },
    
    
    
    populateStkInfo=function(BasicInfo,GlobalMarket){
      StkInfo<-list()
      
      for (i in 1:nrow(BasicInfo)) {
        barsize<-character()
        duration<-character()
        if(BasicInfo[i,2]=="1F"){barsize<-"1 min"; duration<-"1 D"}
        else if(BasicInfo[i,2]=="5F"){barsize<-"5 mins"; duration<-"1 W"}
        else if(BasicInfo[i,2]=="30F"){barsize<-"30 mins"; duration<-"1 M"}
        else if(BasicInfo[i,2]=="daily"){barsize<-"1 day"; duration<-"1 Y"}
        else if(BasicInfo[i,2]=="weekly"){barsize<-"1 week"; duration<-"2 Y"}
        
        if(GlobalMarket=="US"){
          StkInfo[[paste0(BasicInfo[i,1],BasicInfo[i,2])]]<-cbind(SecurityType="STK",Symb=BasicInfo[i,1],intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                                  endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), GlobalMarket=GlobalMarket)
        }
        else if (GlobalMarket=="China"){
          StkInfo[[paste0(BasicInfo[i,1],BasicInfo[i,2])]]<-cbind(SecurityType="STK",Symb=BasicInfo[i,1],intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                                  endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), GlobalMarket=GlobalMarket)
        }
      }
      return(StkInfo)
    },
    
    
    
    PrepStock=function(tws, RealData){
      for (i in 1:length(.self$StkToBePrepared)) {
        .self$nam<-c(nam,.self$MakeName(.self$StkToBePrepared[[i]][1,]))
        InputFileLoc_Stk<-paste0(getwd(),"/Data/OriginalStockData/", .self$StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], ".csv") #save the clean data
        OutputFileLoc_Stk<-paste0(getwd(),"/Data/", .self$StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], ".csv") #Save the clean data
        CombFileLoc_Stk<-paste0(getwd(),"/CandleStickComb/",.self$StkToBePrepared[[i]][1,"GlobalMarket"], "/", nam[i], "Comb.csv") #read the combined data
        .self$InputCombtxt <- c(InputCombtxt,OutputFileLoc_Stk) #this gives all the input locations of data files for candlestick combination
        .self$OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Stk)  #this gives all the locations of the combined data files
        
        #Prepare the data for plot
        if(RealData==TRUE){
          if(.self$StkToBePrepared[[i]][1,"GlobalMarket"]=="US"){
            .self$Get_Stock(tws,.self$StkToBePrepared[[i]][1,"Symb"],.self$StkToBePrepared[[i]][1,"endDateTime"],.self$StkToBePrepared[[i]][1,"barSize"],.self$StkToBePrepared[[i]][1,"duration"],
                            InputFileLoc_Stk)
          }
          else{
            print("Download your data with Python.")#.self$Get_ChineseStock(Symb=.self$StkToBePrepared[[i]][1,"Symb"][[1]], freq=.self$StkToBePrepared[[i]][1,"intv"][[1]], fileloc=InputFileLoc_Stk)
          }
        }
        
        cat("The following data is: ", nam[i], "\n")
        
        Stk_data<-read.csv(file = InputFileLoc_Stk, header = T)
        Stk_data<-Stk_data[,1:6]
        colnames(Stk_data) <- c("Date", "Open", "High","Low", "Close", "Volume")
        Stk_data <- Stk_data[order(Stk_data$Date, decreasing = TRUE),]
        print(head(Stk_data))
        print(tail(Stk_data))
        write.csv(Stk_data, file = OutputFileLoc_Stk, row.names = FALSE)
        
        .self$DownloadData(nam[i], fileloc=OutputFileLoc_Stk)
        
        if(i<length(.self$StkToBePrepared)){print("Please wait for 20 seconds");Sys.sleep(20)}
        .self$RawDataLocation[nam[i]]<-InputFileLoc_Stk
      }
      
      write.table(.self$InputCombtxt,paste0(getwd(),"/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
    },
    
    
    
    
    Get_Stock=function(tws,Symb,endDateTime,barSize,duration,fileloc){   #this may also get the daily, weekly and monthly data
      #check if data exists already:
      olddata<-try(read.csv(file=fileloc,header = TRUE), stop("No data exists for this stock, please initialize first."))
      toDate<-tail(olddata,1)[,"Index"]
      NMonths<-interval(as.Date(toDate), Sys.time())%/%months(1)+1
      NYears<-interval(as.Date(toDate), Sys.time())%/%years(1)+1
      
      newdata<-list()
      if(barSize=="30 mins"){
        for(i in 1:NMonths){
          Contract<-twsEquity(symbol=Symb)
          SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='1', whatToShow='TRADES') 
          print("Please wait for 20 seconds")
          Sys.sleep(22)
          newdata[[NMonths+1-i]]<-data.frame(Index=as.character(index(SdataNew)),SdataNew,row.names = NULL)
          endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
        }
      }else if(barSize=="1 day"){
        for(i in 1:NYears){
          Contract<-twsEquity(symbol=Symb)
          SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='1', whatToShow='TRADES') 
          print("Please wait for 20 seconds")
          Sys.sleep(22)
          newdata[[NYears+1-i]]<-data.frame(Index=as.character(index(SdataNew)),SdataNew,row.names = NULL)
          endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
        }
      }
      newdata<-do.call(rbind, newdata)
      
      #Detect if stock split is announced:
      pivotdate<-newdata$Index[1]
      fac<-sort(table(as.numeric(newdata[which(newdata$Index==pivotdate),2:5] / olddata[which(olddata$Index==pivotdate),2:5])), 
                decreasing = TRUE)[1]%>%names()%>%as.numeric()
      if(fac!=1){
        if(readline(prompt = "Need to split the stock. Please confirm: Y/N")=="Y"){
          olddata[,2:5]<-fac*olddata[,2:5]}else{stop("Execution stopped.")}
      }else{
        cat("No need to split the stock.", "\n")
      }
      
      newdata<-union(olddata[which(olddata$Index<newdata$Index[1]),],newdata)
      write.csv(newdata, file=fileloc, row.names = FALSE) #this will write the xts data into a csv, which is a dataframe when later imported
    },
    
    
    
    Get_ChineseStock=function(Symb,freq,fileloc){
      if (freq=="daily"){
        TushareAPI <- pro_bar(token="e75513f388ed87cb80c5a75cc2e4f6f8a0e9411073249a8793aee47e") #for Chinese A shares. need package "Tushare".
        Sdata <- TushareAPI(ts_code=Symb, start_date = "20000101", adj = "qfq")
      }else{
        TushareAPI <- pro_api(token="e75513f388ed87cb80c5a75cc2e4f6f8a0e9411073249a8793aee47e") #for Chinese A shares. need package "Tushare".
        Sdata <- TushareAPI(ts_code=Symb, start_date = "20000101", adj = "qfq", api_name = freq)}
      
      Sdata<-na.omit(Sdata)
      Sdata<-Sdata%>%mutate(trade_date = as.character(gsub('^(\\d{4})(\\d{2})(\\d{2})$', '\\1-\\2-\\3', trade_date)), amount=as.numeric(amount))
      Sdata<-Sdata[,c("trade_date","open","high","low","close","amount")]
      colnames(Sdata) <- c("Date", "Open", "High","Low", "Close", "Volume")
      Sdata$Date <- as.character(Sdata$Date)
      write.csv(Sdata,file = fileloc, row.names = FALSE)
    },
    
    
    
    DownloadData=function(nam, fileloc){
      df <- read.csv(fileloc, header = T)%>%arrange(Date)
      assign(nam, df, envir = .GlobalEnv)
    },
    
    
    
    ReadCombData=function(OutputCombtxt=.self$OutputCombtxt, nam=.self$nam){  #This script read and load the combined data
      for (i in 1:length(OutputCombtxt)) { 
        CombData <- read.csv(OutputCombtxt[i], header = T) 
        CombData <- CombData[order(CombData$Date, decreasing = FALSE),]
        cat("Combined data is imported, please check the combined data!", "\n", "The following data is: ",nam[i], "\n")
        print(head(CombData))
        print(tail(CombData))
        assign(nam[i], CombData, envir = .GlobalEnv)
      }
    },
    
    
    
    MakeName=function(DataInfo){
      if (DataInfo["SecurityType"]=="FUT") {   #give the name of the security and its corresponding file names
        namresult <- paste(DataInfo["Symb"],DataInfo["intv"], sep = "")
      }else if(DataInfo["SecurityType"] == "STK"){
        if(DataInfo["intv"]=="daily" | DataInfo["intv"]=="weekly" | DataInfo["intv"]=="monthly"){
          if(DataInfo["GlobalMarket"]=="China"){
            namresult <- paste(DataInfo["Symb"],"_",DataInfo["intv"], sep = "")
          }else{
            namresult <- paste(toupper(DataInfo["Symb"]),"_",DataInfo["intv"], sep = "")
          }
        }
        else{namresult <- paste(toupper(DataInfo["Symb"]),DataInfo["intv"], sep = "")
        }
      }else if(DataInfo["SecurityType"] == "FOREX" ){
        namresult <- paste(toupper(gsub("/","",DataInfo["Symb"])),"_",DataInfo["intv"], sep = "")
      }
      return(namresult)
    }
  )
)