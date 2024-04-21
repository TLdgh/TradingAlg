GetFutInfo<-setRefClass(
  "FutToBePrepared",
  fields = list(FutToBePrepared="list", FUT="character", interval="character",
                InputCombtxt="character",OutputCombtxt="character",RawDataLocation="list",nam="character",
                tws="environment", RealData="logical", TimeZ="character"),
  methods = list(
    initialize=function(FUT,interval,tws,RealData = FALSE){
      #Create the FutToBePrepared attribute
      BasicInfo<-merge(FUT,interval)
      .self$TimeZ<-"America/Toronto"
      .self$FutToBePrepared<-.self$populateFutInfo(BasicInfo)
      
      #Prepare Futures contract information
      .self$InputCombtxt<-character()
      .self$OutputCombtxt<-character()
      .self$RawDataLocation<-list()
      .self$nam<-character()
      .self$PrepFutures(tws, RealData)
    },
    
    
    
    populateFutInfo=function(BasicInfo){
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
                                                          endDateTime=format(with_tz(Sys.time(),tz=TimeZ),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20240621",currency="USD",multiplier="20")}
        else if(BasicInfo[i,1]=="GC"){
          FutInfo[[paste0("GC",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("GC"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                          endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="COMEX",expiry="20211229",currency="USD",multiplier="100")}
        else if(BasicInfo[i,1]=="CAD"){
          FutInfo[[paste0("CAD",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("CAD"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                                           endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20220315",currency="USD",multiplier="100000")}
      }
      return(FutInfo)    
    },
    
    
    
    PrepFutures=function(tws, RealData){
      #this is the expiry date of the available contracts, can be found in IB Description
      NQWExpD <- c("20200320", "20200619", "20200918", "20201218", 
                   "20210319", "20210618", "20210917", "20211217",
                   "20220318", "20220617", "20220916", "20221216",
                   "20230317", "20230616", "20230915", "20231215",
                   "20240315","20240621")
      NQ5FExpD <- NQWExpD
      NQ30FExpD <- NQWExpD
      NQ1HExpD <- NQWExpD
      
      NQExpD <- c("20190920", "20191220",
                  "20200320", "20200619", "20200918", "20201218", 
                  "20210319", "20210618", "20210917", "20211217",
                  "20220318", "20220617", "20220916", "20221216",
                  "20230317", "20230616", "20230915", "20231215",
                  "20240315","20240621") 
      
      
      for (i in 1:length(FutToBePrepared)){
        .self$nam<-c(nam,.self$MakeName(FutToBePrepared[[i]][1,]))
        InputFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/Data/OriginalFuturesData/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #read the original data
        OutputFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/Data/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv") #Save the clean data
        CombFileLoc_Fut<-paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], "Comb.csv") #read the combined data
        .self$InputCombtxt <- c(InputCombtxt,OutputFileLoc_Fut) #this gives all the input locations of data files for candlestick combination
        .self$OutputCombtxt <- c(OutputCombtxt,CombFileLoc_Fut)  #this gives all the locations of the combined data files
        
        if(RealData==TRUE){
          if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
            .self$Get_IntradayFut(tws,FutToBePrepared[[i]][1,"Symb"],FutToBePrepared[[i]][1,"exch"],FutToBePrepared[[i]][1,"expiry"],FutToBePrepared[[i]][1,"currency"],
                                  FutToBePrepared[[i]][1,"multiplier"],FutToBePrepared[[i]][1,"endDateTime"],FutToBePrepared[[i]][1,"barSize"],FutToBePrepared[[i]][1,"duration"],
                                  InputFileLoc_Fut)
          }else{
            cat("You are preparing the continuous futures contract!", "\n")
            if(FutToBePrepared[[i]][1,"intv"]=="5FContinuous"){.self$Get_ContinuousFut(tws,NQ5FExpD)}
            else if(FutToBePrepared[[i]][1,"intv"]=="30FContinuous"){.self$Get_ContinuousFut(tws,NQ30FExpD)}
            else if(FutToBePrepared[[i]][1,"intv"]=="1HContinuous"){.self$Get_ContinuousFut(tws,NQ1HExpD)}
            else if(FutToBePrepared[[i]][1,"intv"]=="Continuous"){.self$Get_ContinuousFut(tws,NQExpD)}
            else if(FutToBePrepared[[i]][1,"intv"]=="WContinuous"){.self$Get_ContinuousFut(tws,NQWExpD)}
          }
        }
        cat("The following data is: ", nam[i], "\n")
        .self$DownloadData(nam[i], InputFileLoc_Fut, OutputFileLoc_Fut)
        if(i<length(FutToBePrepared)){print("Please wait for 20 seconds");Sys.sleep(20)}
        .self$RawDataLocation[nam[i]]<-InputFileLoc_Fut
      }
      
      #check if there's 1H data:
      Exist1H<-which(names(RawDataLocation) %in% c("NQ1H","NQ1HContinuous"))
      if(length(Exist1H)!=0){
        for(i in Exist1H){
          if(FutToBePrepared[[i]][1,"barSize"]!="Continuous"){
            NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4H.csv") #Save the new bar data at this location
            .self$OutputCombtxt[i]<-paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HComb.csv")  #this gives the location of the combined data files
            .self$nam[i]<-paste0(FutToBePrepared[[i]][1,"Symb"],"4H")
          }else{
            NewBarOutputfile<-paste0(getwd(),"/Data/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuous.csv") #Save the new bar data at this location
            .self$OutputCombtxt[i]<-paste0(getwd(),"/CandleStickComb/",FutToBePrepared[[i]][1,"Symb"],"/",FutToBePrepared[[i]][1,"Symb"],"4HContinuousComb.csv")  #this gives the location of the combined data files
            .self$nam[i]<-paste0(FutToBePrepared[[i]][1,"Symb"],"4HContinuous")
          }
          .self$RawDataLocation[nam[i]]<-paste0("/Users/tengli/R/TradingAlg","/Data/OriginalFuturesData/", FutToBePrepared[[i]][1,"Symb"], "/", nam[i], ".csv")
          write.csv(.self$FutNewBarSize(DataFile=RawDataLocation[[i]], intv="1H", barSize = 4), file=NewBarOutputfile, row.names = FALSE)
          .self$InputCombtxt[i]<-NewBarOutputfile#this gives the input location of data files for candlestick combination app
        }
      }
      
      write.table(InputCombtxt,paste0("/Users/tengli/R/TradingAlg","/CandleStickComb/InputLoc.txt"),sep="\n",col.names=FALSE, row.names=FALSE,quote = FALSE)
    },
    
    
    
    #to find the contract details like conId, go to IB Watchlist, right click contract name and click Financial Instrument Info and select Detail
    #see duration and barsize in https://interactivebrokers.github.io/tws-api/historical_limitations.html#pacing_violations
    Get_IntradayFut=function(tws,Symb,exch,expiry,currency,multiplier,endDateTime,barSize,duration,fileloc){
      Contract<-twsFuture(symbol=Symb,exch=exch, expiry=expiry, currency=currency, multiplier=multiplier, include_expired="1")
      Fdata<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize=barSize, duration=duration, useRTH='0', whatToShow='TRADES') 
      Index=format(as.POSIXct(index(Fdata),tz=TimeZ),"%Y-%m-%d %H:%M:%S")
      write.csv(data.frame(Index=Index, Fdata), file=fileloc, row.names = FALSE) #this will write the xts data into a csv, which is a dataframe when later imported
    },
    
    
    
    ######################################################### Import new data downloaded from IB_API, and make continous contract
    #We use the backwards ratio method to adjust rollover. The rolling point chosen is the First Notice Date. For GC or HG, this date is 
    #the last business day of the month prior to the contract month. If the business day coincide with the Bank Holiday, choose the previous
    #business day. For example, GCJune2021, the business day is May31, but it's a holiday, so FND = May28.
    #For 6C, rolling point is the Settlement Date. The Settlement Date is the Tuesday of the third week of the contract month.
    #For equity index futures, rolling point is the Settlement Date. The Settlement Date is the Friday of the third week of the contract month.
    
    #to find the contract details like conId, go to IB Watchlist, right click contract name and click Financial Instrument Info and select Detail
    #see duration and barsize in https://interactivebrokers.github.io/tws-api/historical_limitations.html#pacing_violations
    CombineContracts=function(OldF, NewF){
      colnames(OldF) <- c("Date", "Open", "High","Low", "Close", "Volume", "WAP", "GAPS", "Count", "FND")
      colnames(NewF) <- c("Date", "Open", "High","Low", "Close", "Volume", "WAP", "GAPS", "Count", "FND")
      RollPoint  <- unique(OldF$FND)
      AdjRate <- Cl(filter(NewF, Date==RollPoint))/Cl(filter(OldF, Date==RollPoint))
      OldF<- data.frame("Date"=OldF$Date, round(OldF[,c(2:5)]*AdjRate,5), OldF[,c(6:10)])
      
      VolAdj<-subset(merge(OldF[,c(1,6)], NewF[,c(1,6)], by= "Date"),Date<=RollPoint) #sum the volume of the two contracts that have overlaps before the rollpoint
      OldF[match(VolAdj$Date,OldF$Date,),]$Volume <- rowSums(VolAdj[,2:3])
      NewF[which(NewF$Date==RollPoint),]$Volume <- OldF[which(OldF$Date==RollPoint),]$Volume
      ContinuousFut <- rbind(filter(OldF, as.POSIXct(Date,tz=TimeZ) < as.POSIXct(RollPoint,tz=TimeZ)), filter(NewF, as.POSIXct(Date, tz=TimeZ) >= as.POSIXct(RollPoint,tz=TimeZ)))
      ContinuousFut$FND <- unique(NewF$FND)
      return(ContinuousFut)
    },
    
    
    
    Get_ContinuousFut=function(tws, ExpD){
      FUT_NQ<-list()
      for(i in 1:length(ExpD)){
        possibleError<-tryCatch(
          expr={
            if(deparse(substitute(ExpD))=="NQExpD"){
              NQtitle<-paste0("NQ_", ExpD[i])
              fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
              NQ<-read.csv(file = fileloc, header=T)
              exp<-as.POSIXct(ExpD[i], format="%Y%m%d", tz=TimeZ)
              latesttime<-ifelse("latesttime" %in% colnames(NQ), last(NQ$latesttime), as.POSIXct(last(NQ)$Index, format="%Y-%m-%d", tz=TimeZ))
              
              barSize<-"1 day"
              saveloc<-paste0(getwd(), "/Data/OriginalFuturesData/NQ/NQContinuous.csv")
            }else if(deparse(substitute(ExpD))=="NQWExpD"){
              NQtitle<-paste0("NQW_", ExpD[i])
              fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
              NQ<-read.csv(file = fileloc, header=T)
              exp<-as.POSIXct(ExpD[i], format="%Y%m%d", tz=TimeZ)
              latesttime<-ifelse("latesttime" %in% colnames(NQ), last(NQ$latesttime), as.POSIXct(last(NQ)$Index, format="%Y-%m-%d", tz=TimeZ))
              
              barSize<-"1 week"
              saveloc<-paste0(getwd(), "/Data/OriginalFuturesData/NQ/NQWContinuous.csv")
            }else if(deparse(substitute(ExpD))=="NQ5FExpD"){
              NQtitle<-paste0("NQ5F_", ExpD[i])
              fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
              NQ<-read.csv(file = fileloc, header=T)
              exp <- as.POSIXct(paste(ExpD[i], "00:00:00"), format="%Y%m%d %H:%M:%S", tz=TimeZ)-minutes(5)
              latesttime<-ifelse("latesttime" %in% colnames(NQ), last(NQ$latesttime), as.POSIXct(last(NQ)$Index, format="%Y-%m-%d %H:%M:%S", tz=TimeZ))
              
              barSize<-"5 mins"
              saveloc<-paste0(getwd(), "/Data/OriginalFuturesData/NQ/NQ5FContinuous.csv")
            }else if(deparse(substitute(ExpD))=="NQ30FExpD"){
              NQtitle<-paste0("NQ30F_", ExpD[i])
              fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
              NQ<-read.csv(file = fileloc, header=T)
              exp <- as.POSIXct(paste(ExpD[i], "00:00:00"), format="%Y%m%d %H:%M:%S", tz=TimeZ)-minutes(30)
              latesttime<-ifelse("latesttime" %in% colnames(NQ), last(NQ$latesttime), as.POSIXct(last(NQ)$Index, format="%Y-%m-%d %H:%M:%S", tz=TimeZ))
              
              barSize<-"30 mins"
              saveloc<-paste0(getwd(), "/Data/OriginalFuturesData/NQ/NQ30FContinuous.csv")
            }else if(deparse(substitute(ExpD))=="NQ1HExpD"){
              NQtitle<-paste0("NQ1H_", ExpD[i])
              fileloc<-paste0(getwd(),"/Data/OriginalFuturesData/NQ/Continuous/", NQtitle, ".csv")
              NQ<-read.csv(file = fileloc, header=T)
              exp <- as.POSIXct(paste(ExpD[i], "00:00:00"), format="%Y%m%d %H:%M:%S", tz=TimeZ)-hours(1)
              latesttime<-ifelse("latesttime" %in% colnames(NQ), last(NQ$latesttime), as.POSIXct(last(NQ)$Index, format="%Y-%m-%d %H:%M:%S", tz=TimeZ))
              
              barSize<-"1 hour"
              saveloc<-paste0(getwd(), "/Data/OriginalFuturesData/NQ/NQ1HContinuous.csv")
            }else{stop("You need to update the Get_ContinuousFut function.")}
          },
          error=function(e){
            cat("We have an error: ", conditionMessage(e), "\n")
            warning(e)
            stop("There is no csv file for the expiration date, please initialize the file.")}
        )
        
        if(latesttime<=exp){
          print(paste("Data", NQtitle, "needs to be updated."))
          proceed<-readline(prompt="You're updating an existing data. Are you really sure you want to overwrite? Y/N: ")
          if(proceed=="Y"){
            cat("Downloading new data", NQtitle, ".....", "\n")
            
            endtime<-paste(ExpD[i], "00:00:00")
            twsNQ <- twsFuture(symbol = "NQ",exch="CME", expiry=ExpD[i], currency="USD", multiplier = "20", include_expired="1")
            NQ<- reqHistoricalData(tws, Contract=twsNQ, endDateTime=endtime, barSize=barSize, duration='4 M', useRTH='0', whatToShow='TRADES')
            
            if(deparse(substitute(ExpD)) %in% c("NQExpD","NQWExpD")){
              Index<-format(index(NQ), "%Y-%m-%d")
              latesttime<-format(with_tz(Sys.time(),tz=TimeZ),"%Y-%m-%d")
            }else{
              Index<-format(index(NQ), "%Y-%m-%d %H:%M:%S")
              latesttime<-format(with_tz(Sys.time(),tz=TimeZ),"%Y-%m-%d %H:%M:%S")
            }
            
            NQ<-data.frame(Index=Index, NQ, latesttime=latesttime)
            print(filter(NQ, hour(as.POSIXct(NQ$Index))<=1))
            
            proceed<-readline(prompt="Does the data have a problem on the format of the date? Y/N: ")
            if(proceed=="N"){
              write.csv(NQ, file=fileloc, row.names = FALSE) #this will write the xts data into a csv, which is a dataframe when later imported
              NQ<-read.csv(file = fileloc, header=T)
            }else{stop("Data downloading process has a problem. Please fix the issue first on the source code level.")}
          }else{stop("Downloading stopped.")}
        }else{print(paste(NQtitle, "has been previously downloaded and does not require update."))}
        
        NQ$FND<-last(NQ$Index)
        if("latesttime" %in% colnames(NQ)){FUT_NQ[[NQtitle]]<-select(NQ, -"latesttime")}else{FUT_NQ[[NQtitle]]<-NQ}
      }
      
      cat("\n", "The complete contract looks like:--------------------------------------------------------", "\n")
      ContinuousFut<-.self$CombineContracts(FUT_NQ[[1]], FUT_NQ[[2]])
      for(i in 3:(length(FUT_NQ)-1)){ContinuousFut<-.self$CombineContracts(ContinuousFut, FUT_NQ[[i]])}
      print(head(ContinuousFut,30))
      print(tail(ContinuousFut,30))
      ContinuousFut<-.self$CombineContracts(ContinuousFut, FUT_NQ[[length(FUT_NQ)]])
      proceed<-readline(prompt="Has the data gotten combined correctly, and do you want to save the new data? Y/N: ")
      if(proceed=="Y"){
        write.csv(ContinuousFut, file=saveloc, row.names = FALSE) 
      }
    },
    
    
    
    DownloadData=function(nam, InputFileLoc_Fut, OutputFileLoc_Fut){
      Fut_data<-read.csv(file = InputFileLoc_Fut, header = T)
      Fut_data<-Fut_data[,1:6]
      colnames(Fut_data) <- c("Date", "Open", "High","Low", "Close", "Volume")
      Fut_data <- Fut_data[order(Fut_data$Date, decreasing = TRUE),]
      print(head(Fut_data))
      print(tail(Fut_data))
      write.csv(Fut_data, file = OutputFileLoc_Fut, row.names = FALSE)
    },
    
    
    
    FutNewBarSize=function(DataFile, intv=NULL, barSize=NULL){
      DataInput <- read.csv(file = DataFile, header = T) #get the futures data
      DataInput <- rbind(DataInput, last(DataInput)) #use the last time as a ghost bar
      DataInput$Index[nrow(DataInput)]<-as.character(as.POSIXct(DataInput$Index[nrow(DataInput)], format="%Y-%m-%d %H:%M:%S", tz=TimeZ)+hours(1)) #change the time for the ghost bar
      
      if(intv %in% c("1H","1HContinuous")){ #currently only support conversion from 1H to 4H
        barintv<-c(which(format(as.POSIXct(DataInput[,1], tz=TimeZ), "%H:%M:%S")=="18:00:00"), nrow(DataInput)) #get the interval from 18:00:00 everyday
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
            namresult <- paste(DataInfo["A_STOK"],"_",DataInfo["intv"], sep = "")
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


