PickModel<-function(PriceData, Test=FALSE){   #this gives the information of every complete planet, including data, class, slope etc.
  source("/Users/tengli/R/Script/ChanLunFunction.R")
  
  StarData <- StarFunction(PriceData)
  Bi<- BiFunction(StarData)
  Planets<-subset(as.data.frame(PlanetFunction(Bi)), PlanetHigh!=0)
  PreparedMACDData<-PricedataMACD(PriceData) #calculate the MACD
  PreparedMFIData<-PricedataMFI(PriceData) #calculate the MACD
  PreparedBOLLData<-PricedataBOLL(PriceData) #calculate the BOLL
  
  Result<-list()
  ModelInfo<-list()
  
  #the following check if we have enough Bi to run the loop to the last planet, if not then discard the last planet
  if(Test==FALSE){
    if(which(Bi$SLOPE==subset(Bi, Bi$BiStartD==tail(Planets,1)$PlanetEndD)$SLOPE & Bi$BiStartD==subset(Bi, Bi$BiStartD==tail(Planets,1)$PlanetEndD)$BiStartD)+5<=nrow(Bi)){LoopLimit<-nrow(Planets)}else{LoopLimit<-nrow(Planets)-1}
  }else{LoopLimit<-nrow(Planets)} #if we want to get the model of the test data, we don't want to put limit
  
  for(i in 1:LoopLimit){
    Modelrange<-list()
    InPlanetBi<-subset(Bi, Bi$BiEndD==Planets[i,]$PlanetStartD)
    OutPlanetBi<-subset(Bi, Bi$BiStartD==Planets[i,]$PlanetEndD)
    InIndex<-which(Bi$SLOPE==InPlanetBi$SLOPE & Bi$BiStartD==InPlanetBi$BiStartD)
    OutIndex<-which(Bi$SLOPE==OutPlanetBi$SLOPE & Bi$BiStartD==OutPlanetBi$BiStartD)
    SLOPE<-InPlanetBi$SLOPE
    PlanetHigh<-Planets[which(Planets$PlanetStartD==InPlanetBi$BiEndD&Planets$PlanetEndD==OutPlanetBi$BiStartD),]$PlanetHigh
    PlanetLow<-Planets[which(Planets$PlanetStartD==InPlanetBi$BiEndD&Planets$PlanetEndD==OutPlanetBi$BiStartD),]$PlanetLow
    NumOfBi<-OutIndex-InIndex-1
    
    if (SLOPE==1){
      if(NumOfBi>=9){L1<-1}else{L1<-0} #有没有9笔及以上
      
      BiMin<-subset(Bi, index(Bi)>InIndex&index(Bi)<OutIndex)$MIN
      BiMax<-subset(Bi, index(Bi)>InIndex&index(Bi)<OutIndex)$MAX
      if(any(BiMin>PlanetHigh)==TRUE & any(BiMax<PlanetLow)==FALSE){L2<-1} #上扩展
      else if(any(BiMin>PlanetHigh)==FALSE & any(BiMax<PlanetLow)==TRUE){L2<-2} #下扩展
      else if(any(BiMin>PlanetHigh)==FALSE & any(BiMax<PlanetLow)==FALSE){L2<-0} #没有扩展
      else{L2<-3}#上下扩展都有 
      
      if(OutIndex+2<=nrow(Bi)){ #如果 Test==TRUE 则要么有三买要么没有，根据选择的数据是否正确。如果Test==FALSE,则不影响
        if(Bi[OutIndex+2,]$MAX<Bi[OutIndex,]$MAX){L3<-0}else{L3<-1}#有没有新笔出新高且三买,有是1，没有是0
      }
      else{L3<-0} 
      
      if(Test==FALSE){ #也就是仅仅测试历史走势，不对当下数据Test==TRUE做分类
        if(L3==0){         #无新高
          Rev1<-Bi[OutIndex+1,]
          Rev2<-Bi[OutIndex+3,]
          if(Rev1$MIN<PlanetHigh & Rev1$MIN>=PlanetLow){L4<-1}  #回调一笔回中枢区间
          else if(Rev1$MIN<PlanetLow & Rev1$MIN>=Bi[InIndex,]$MIN){L4<-2}  #回调一笔跌破中枢下沿
          else if(Rev1$MIN<Bi[InIndex,]$MIN){L4<-3}   #回调一笔跌破中枢走势起点
          else if(Rev1$MIN>PlanetHigh & Rev2$MIN<PlanetHigh & Rev2$MIN>=PlanetLow){L4<-4}   #回调一笔没回中枢但第二笔回， 且第二笔没有新高出新中枢
          else if(Rev1$MIN>PlanetHigh & Rev2$MIN<PlanetLow & Rev2$MIN>=Bi[InIndex,]$MIN){L4<-5}   #回调一笔没回中枢但第二笔跌破中枢下沿，且第二笔没有新高出新中枢
          else if(Rev1$MIN>PlanetHigh & Rev2$MIN<Bi[InIndex,]$MIN){L4<-6}   #回调一笔没回中枢但第二笔跌破中枢走势起点，且第二笔没有新高出新中枢
          else{L4<-7} #比如没有新高但是也没有回中枢区间，最后在中枢上沿震荡出新的上升或下降中枢
        }
        else if(L3==1){         #有三买且新高
          Rev1<-Bi[OutIndex+2+1,]
          Rev2<-Bi[OutIndex+2+3,]
          if(Rev1$MIN<PlanetHigh & Rev1$MIN>=PlanetLow){L4<-1} #回调一笔回中枢区间
          else if(Rev1$MIN<PlanetLow & Rev1$MIN>=Bi[InIndex,]$MIN){L4<-2}#回调一笔跌破中枢下沿
          else if(Rev1$MIN<Bi[InIndex,]$MIN){L4<-3}#回调一笔跌破中枢走势起点
          else if(Rev1$MIN>PlanetHigh & Rev2$MAX<Bi[OutIndex+2,]$MAX & Rev2$MIN<PlanetHigh & Rev2$MIN>=PlanetLow){L4<-4} #回调一笔没回中枢但第二笔回，且第二笔没有新高出新中枢
          else if(Rev1$MIN>PlanetHigh & Rev2$MAX<Bi[OutIndex+2,]$MAX & Rev2$MIN<PlanetLow & Rev2$MIN>=Bi[InIndex,]$MIN){L4<-5}#回调一笔没回中枢但第二笔跌破中枢下沿，且第二笔没有新高出新中枢
          else if(Rev1$MIN>PlanetHigh & Rev2$MAX<Bi[OutIndex+2,]$MAX & Rev2$MIN<Bi[InIndex,]$MIN){L4<-6}#回调一笔没回中枢但第二笔跌破中枢走势起点，且第二笔没有新高出新中枢
          else{L4<-7}        #比如新高后没有回中枢区间，最终在中枢上沿震荡出新的上升或下降中枢
        }
      }
    }
    else if(SLOPE==-1){
      if(NumOfBi>=9){L1<-1}else{L1<-0} #有没有9笔及以上
      
      BiMin<-subset(Bi, index(Bi)>InIndex&index(Bi)<OutIndex)$MIN
      BiMax<-subset(Bi, index(Bi)>InIndex&index(Bi)<OutIndex)$MAX
      if(any(BiMin>PlanetHigh)==TRUE & any(BiMax<PlanetLow)==FALSE){L2<-1} #上扩展
      else if(any(BiMin>PlanetHigh)==FALSE & any(BiMax<PlanetLow)==TRUE ){L2<-2} #下扩展
      else if(any(BiMin>PlanetHigh)==FALSE & any(BiMax<PlanetLow)==FALSE){L2<-0} #没有扩展
      else{L2<-3}#上下扩展都有 
      
      if(OutIndex+2<=nrow(Bi)){ #如果 Test==TRUE 则要么有三买要么没有，根据选择的数据是否正确。如果Test==FALSE,则不影响
        if(Bi[OutIndex+2,]$MIN>Bi[OutIndex,]$MIN){L3<-0}else{L3<-1}#有没有新笔出新高且三买,有是1，没有是0
      }
      else{L3<-0}                               
      
      if (Test==FALSE){           #也就是仅仅测试历史走势，不对当下数据Test==TRUE做分类
        if(L3==0){              #无新低
          Rev1<-Bi[OutIndex+1,]
          Rev2<-Bi[OutIndex+3,]
          if(Rev1$MAX>PlanetLow & Rev1$MAX<=PlanetHigh){L4<-1}   #反弹一笔回中枢区间
          else if(Rev1$MAX>PlanetHigh & Rev1$MAX<=Bi[InIndex,]$MAX){L4<-2}   #反弹一笔升破中枢上沿
          else if(Rev1$MAX>Bi[InIndex,]$MAX){L4<-3}   #反弹一笔升破中枢走势起点
          else if(Rev1$MAX<PlanetLow & Rev2$MAX>PlanetLow & Rev2$MAX<=PlanetHigh){L4<-4}   #反弹一没笔回中枢但第二笔回，且第二笔没有新低出新中枢
          else if(Rev1$MAX<PlanetLow & Rev2$MAX>PlanetHigh & Rev2$MAX<=Bi[InIndex,]$MAX){L4<-5}   #反弹一笔没笔回中枢但第二笔升破中枢上沿，且第二笔没有新低出新中枢
          else if(Rev1$MAX<PlanetLow & Rev2$MAX>Bi[InIndex,]$MAX){L4<-6}   #反弹一笔没笔回中枢但第二笔升破中枢走势起点，且第二笔没有新低出新中枢
          else{L4<-7}   #比如没有新低但是也没有回中枢区间，最后在中枢下沿震荡出新的上升或下降中枢
        }
        else if(L3==1){           #有三卖且新低
          Rev1<-Bi[OutIndex+2+1,]
          Rev2<-Bi[OutIndex+2+3,]
          if(Rev1$MAX>PlanetLow & Rev1$MAX<=PlanetHigh){L4<-1} #反弹一笔回中枢区间
          else if(Rev1$MAX>PlanetHigh & Rev1$MAX<=Bi[InIndex,]$MAX){L4<-2}#反弹一笔升破中枢上沿
          else if(Rev1$MAX>Bi[InIndex,]$MAX){L4<-3}#反弹一笔升破中枢走势起点
          else if(Rev1$MAX<PlanetLow & Rev2$MIN>Bi[OutIndex+2,]$MIN & Rev2$MIN>PlanetLow & Rev2$MIN<=PlanetHigh){L4<-4} #反弹一笔没回中枢但第二笔回，且第二笔没有新低出新中枢
          else if(Rev1$MIN<PlanetLow & Rev2$MIN>Bi[OutIndex+2,]$MIN & Rev2$MIN>PlanetHigh & Rev2$MIN<=Bi[InIndex,]$MAX){L4<-5}#反弹一笔没回中枢但第二笔升破中枢上沿，且第二笔没有新低出新中枢
          else if(Rev1$MIN<PlanetLow & Rev2$MIN>Bi[OutIndex+2,]$MIN & Rev2$MIN>Bi[InIndex,]$MAX){L4<-6}#反弹一笔没回中枢但第二笔升破中枢走势起点，且第二笔没有新低出新中枢
          else{L4<-7}   #比如新低后没有回中枢区间，最终在中枢下沿震荡出新的上升或下降中枢
        }
      }
    }
    
    if (Test==FALSE & L3==0){
      Modelrange[["Data"]]<-subset(PriceData, Date>=InPlanetBi$BiStartD)%>%subset(Date<=OutPlanetBi$BiEndD)
      Modelrange[["Class"]]<- paste0(L1,L2,L3,L4)
    }
    else if(Test==FALSE & L3==1){      
      Modelrange[["Data"]]<-subset(PriceData, Date>=InPlanetBi$BiStartD)%>%subset(Date<=Bi[OutIndex+2,]$BiEndD)
      Modelrange[["Class"]]<- paste0(L1,L2,L3,L4)
    }
    else if(Test==TRUE & L3==0){
      Modelrange[["Data"]]<-subset(PriceData, Date>=InPlanetBi$BiStartD)%>%subset(Date<=OutPlanetBi$BiEndD)
      Modelrange[["Class"]]<- sapply(1:7, function(j) paste0(sapply(0:1, function(i) paste0(L1,L2,i)), j))   #要测试当下数据后的14种可能性
      
    }else if(Test==TRUE & L3==1){
      Modelrange[["Data"]]<-subset(PriceData, Date>=InPlanetBi$BiStartD)%>%subset(Date<=Bi[OutIndex+2,]$BiEndD)
      Modelrange[["Class"]]<- sapply(1:7, function(j) paste0(L1,L2,L3, j))            #由于L3==1，只需测试当下数据的7种可能性
    }
    
    Modelrange[["SLOPE"]]<-SLOPE
    Modelrange[["PlanetLow"]]<-PlanetLow
    Modelrange[["PlanetHigh"]]<-PlanetHigh
    Modelrange[["InIndex"]]<-InIndex
    Modelrange[["OutIndex"]]<-OutIndex
    
    valDates<-Planets[i,]$PlanetEndD
    Pdate<-StarData[which(StarData$Date==valDates)+2,"Date"]
    Modelrange<-c(Modelrange, MACDPower(DataToBeTested=subset(PriceData,Date<=Pdate), BarOverride=FALSE,PreparedMACDData=PreparedMACDData, PreparedMFIData=PreparedMFIData,PreparedBOLLData=PreparedBOLLData))
    
    ModelInfo<-c(ModelInfo, list(Modelrange))
  }
  return(ModelInfo)
}




CreateClassData<-function(ModelInfo){ #this creates the main class that contains the sub classes
  MainClassData<-sapply(ModelInfo, function(x){y<-list(x);names(y)<-make.names(x$Class);return(y) } ) #give each sublist the name according to its class
  ClassName<-names(MainClassData)
  MainClassData<-sapply(unique(ClassName), function(x){list(MainClassData[names(MainClassData)==x]) }, USE.NAMES = TRUE) #regroup according to the same name
  MainClassData<-lapply(MainClassData, function(x){setNames(x, nm=make.names(names(x),unique = TRUE) )}) #make the sublists name unique
  return(MainClassData)
}




FitModel<-function(MainClassData,ClassData){
  CreateSignal<-function(x){
    MACD<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["MACD"]]})); colnames(y)<-c("能量背驰","面积背驰","比例背驰","能量强度","面积强度","比例强度"); return(y) })
    MFI<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["MFI"]]})); colnames(y)<-c("MFI背驰","MFI强度","量比"); return(y) })
    Str<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["形态背驰"]]})); colnames(y)<-c("形态背驰","形态强度"); return(y) })
    BOLL<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["BOLL信号"]]})); colnames(y)<-c("BOLL方向","BOLL强度"); return(y) })
    Candle<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["Other"]]})); colnames(y)<-c("启动K线排名","分型强度"); return(y) })
    Result<-list(MACD=MACD,MFI=MFI,Str=Str,BOLL=BOLL,Candle=Candle) 
    return(Result)}
  
  ModelResult<-CreateSignal(MainClassData)
  DataResult<-CreateSignal(ClassData)
  
  
  FitMACD<-map(ModelResult$MACD, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$MACD[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitMFI<-map(ModelResult$MFI, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$MFI[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitStr<-map(ModelResult$Str, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$Str[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitBOLL<-map(ModelResult$BOLL, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$BOLL[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitCandle<-map(ModelResult$Candle, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$Candle[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  Fit<-tibble(FitMACD=FitMACD,FitMFI=FitMFI,FitStr=FitStr,FitBOLL=FitBOLL,FitCandle=FitCandle) 
  
  GlobalResult<-sapply(Fit, function(x){sapply(x, min)})%>%as.data.frame()%>%mutate(Total=rowSums(.)) #min because we look for closest similarities
  
  return(GlobalResult)
}




MainBootstrap<-function(DataToBeFit,OriginalData,nboot){ #create results and combine and average them
  ModelInfo<-PickModel(OriginalData) #contains 1-level sublist of all planets
  DataInfo<-PickModel(DataToBeFit) #should contain only 1-level sublist of one planet, should have the same structure as ModelInfo
  
  ModelInfoSlope<-sapply(1:length(ModelInfo), function(i) (ModelInfo[[i]]$SLOPE))
  DataInfoSlope<-as.vector(DataInfo[[1]]$SLOPE)
  
  ModelID<-which(ModelInfoSlope %in% DataInfoSlope)
  if (length(ModelID)==0){
    stop("There is no planet model for the data to be tested! Try with a new original data.")
  }
  SelectedModel<-lapply(ModelID, function(x) (ModelInfo[[x]]))
  
  MainClassData<-CreateClassData(SelectedModel) #simulate nboot number of data for each SelectedModel
  ClassData<-CreateClassData(DataInfo)
  ClassFitResult<-FitModel(MainClassData=MainClassData,ClassData=ClassData)
  
  FitResult<-lapply(1:length(ClassFitResult), function(i){apply(matrix(as.numeric(do.call(cbind, ClassFitResult[[i]])),byrow=FALSE, nrow=1),1,max)}   )  #take the maximum value among each class of model to avoid penalty, the nrow is the number of indicators from FitModel
  FitResult<-lapply(1:length(FitResult), function(x){lapply(FitResult[x], setNames, c("ModelNumber")   )})
  names(FitResult)<-names(MainClassData)
  
  return(FitResult) #FitResult gives the overall averaged result within each Class
}




ClassDataToCSV<-function(ClassData){ #this converts each Class into a csv file. nrow=number of simulation, ncol = number of data from all the models/planets in a Class
  ZS<-data.frame()
  ZScolname<-c()
  for (i in 1:length(ClassData)){
    for (j in 1:ncol(ClassData[[i]])){ZScolname<-c(ZScolname, paste("ZS",i, "_", j, sep = ""))}
  }
  ZS<-do.call(cbind,ClassData) 
  colnames(ZS)<-ZScolname
  
}




BootStrap<-function(DataToBeFit,OriginalData, nboot=1000){
  source("/Users/tengli/R/Script/ChanLunFunction.R")
  
  StarData <- StarFunction(DataToBeFit)
  Bi<- BiFunction(StarData)
  Planets<-subset(as.data.frame(PlanetFunction(Bi)), PlanetHigh!=0)
  LastPlanet<-tail(Planets,1)
  InPlanetBi<-subset(Bi, Bi$BiEndD==LastPlanet$PlanetStartD)
  OutPlanetBi<-subset(Bi, Bi$BiStartD==LastPlanet$PlanetEndD)
  InIndex<-which(Bi$SLOPE==InPlanetBi$SLOPE & Bi$BiStartD==InPlanetBi$BiStartD)
  OutIndex<-which(Bi$SLOPE==OutPlanetBi$SLOPE & Bi$BiStartD==OutPlanetBi$BiStartD)
  
  DataToBeFit<-subset(DataToBeFit, Date>=Bi[InIndex-1,]$BiStartD & Date<=Bi[OutIndex,]$BiEndD) #get the last planet data
  
  OriginalData<-subset(OriginalData, Date<=Bi[InIndex,]$BiStartD) #truncate the original data so it doesn't contain DataToBeFit
  
  FitResult<-MainBootstrap(DataToBeFit=DataToBeFit,OriginalData=OriginalData,nboot=nboot)
  BootFinalResult<-sapply(unique(names(unlist(FitResult))), function(x) sum(unlist(FitResult)[x==names(unlist(FitResult))]))
  BootFinalResult<-setNames(object=BootFinalResult,nm=unique(names(unlist(BootFinalResult))))
  BootFinalResult<-split(BootFinalResult, c("ModelNumber"))
  
  FinalCI<-sort(BootFinalResult$ModelNumber)
  print(FinalCI)
  
  Wdata<-data.frame(FinalCI)
  Wdata$Class<-substr(rownames(Wdata),start=1,stop=3)
  W0<-subset(Wdata, substr(Class,3,3)=="0")[,1] #get the ones with L3=0
  W1<-subset(Wdata,substr(Class,3,3)=="1")[,1] #get the ones with L3=1
  
  tryCatch(
    expr={MWTest<-c(W0,W1)  ;print(MWTest)},
    error=function(e){message("Something went wrong!")}
  )
  
}




SimTrend<-function(Data,n,SeparateSim=FALSE,CombineSim=FALSE){
  if(n>4){stop("Cannot plot more than three levels!")}
  #Initiate the BeginTrend dataframe consisting 3 rows of data
  BeginTrend<-BiFunction(StarData = StarFunction(Data))
  BeginTrend<-tail(BeginTrend,3)
  P1<-BeginTrend%>%mutate(Ind=index(BeginTrend))%>%filter(SLOPE==1)%>%select(Price=MIN,Ind)
  P2<-BeginTrend%>%mutate(Ind=index(BeginTrend))%>%filter(SLOPE==-1)%>%select(Price=MAX,Ind)
  #Convert BeginTrend into line data
  SimResult<-list(Level1=arrange(rbind(P1,P2), Ind)[,c("Ind","Price")])
  SimResult[["Level1"]][4,]<-c(4,anti_join(gather(BeginTrend[,c("MIN","MAX")],key="Ind",value = "Price"), SimResult[[1]], by=c("Price"="Price"))$Price)
  BeginTrend<-list(Point1=BeginTrend[,1:3])#make it a list
  
  PointsConverter<-function(Point,Targetframe){#convert the line data back into BeginTrend format
    Direction<-Targetframe[nrow(Targetframe)-1,"SLOPE"]
    if(Direction==1){
      Result<-rbind(Targetframe, data.frame(SLOPE=Direction, MIN=tail(Targetframe,1)$MIN, MAX=tail(Point,1)$Price))
    }else{
      Result<-rbind(Targetframe, data.frame(SLOPE=Direction, MIN=tail(Point,1)$Price, MAX=tail(Targetframe,1)$MAX))
    }
    return(Result)
  }
  
  GetPoints<-function(BeginTrend){
    AllPoints<-list()
    for(j in 1:length(BeginTrend)){
      LastBi<-nrow(BeginTrend[[j]])
      
      P1<-BeginTrend[[j]]%>%mutate(Ind=index(BeginTrend[[j]]))%>%filter(SLOPE==1)%>%select(Price=MIN,Ind)
      P2<-BeginTrend[[j]]%>%mutate(Ind=index(BeginTrend[[j]]))%>%filter(SLOPE==-1)%>%select(Price=MAX,Ind)
      tempResult<-list(Level1=arrange(rbind(P1,P2), Ind)[,c("Ind","Price")])
      tempResult[["Level1"]][LastBi+1,]<-c(LastBi+1,anti_join(gather(BeginTrend[[j]][,c("MIN","MAX")],key="Ind",value = "Price"), tempResult[[1]], by=c("Price"="Price"))$Price)
      
      Direction<-BeginTrend[[j]][LastBi-1,"SLOPE"]
      Point1<-data.frame()
      Point2<-data.frame()
      Point3<-data.frame()
      Point4<-data.frame()
      Point5<-data.frame()
      Point6<-data.frame()
      Point7<-data.frame()
      Point8<-data.frame()
      Point9<-data.frame()
      Point10<-data.frame()
      
      if(Direction==1){
        if(BeginTrend[[j]][LastBi, "MIN"]<BeginTrend[[j]][LastBi-1,"MIN"]){
          Point1<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=abs(BeginTrend[[j]][LastBi-1, "MIN"]-BeginTrend[[j]][LastBi, "MIN"])/2+BeginTrend[[j]][LastBi,"MIN"]))
          Point2<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=abs(BeginTrend[[j]][LastBi-1, "MAX"]-BeginTrend[[j]][LastBi-1, "MIN"])/2+BeginTrend[[j]][LastBi-1,"MIN"]))
          Point3<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=abs(BeginTrend[[j]][LastBi-1, "MAX"]-BeginTrend[[j]][LastBi-1, "MIN"])/2+BeginTrend[[j]][LastBi-1,"MAX"]))
        }else{
          Point4<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=abs(BeginTrend[[j]][LastBi, "MAX"]-BeginTrend[[j]][LastBi, "MIN"])/2+BeginTrend[[j]][LastBi,"MIN"]))
          Point5<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=abs(BeginTrend[[j]][LastBi, "MAX"]-BeginTrend[[j]][LastBi, "MIN"])/2+BeginTrend[[j]][LastBi,"MAX"]))
        }
      }else{
        if(BeginTrend[[j]][LastBi, "MAX"]>BeginTrend[[j]][LastBi-1,"MAX"]){
          Point6<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=BeginTrend[[j]][LastBi,"MAX"]-abs(BeginTrend[[j]][LastBi-1, "MAX"]-BeginTrend[[j]][LastBi, "MAX"])/2))
          Point7<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=BeginTrend[[j]][LastBi-1,"MAX"]-abs(BeginTrend[[j]][LastBi-1, "MAX"]-BeginTrend[[j]][LastBi-1, "MIN"])/2))
          Point8<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=BeginTrend[[j]][LastBi-1,"MIN"]-abs(BeginTrend[[j]][LastBi-1, "MAX"]-BeginTrend[[j]][LastBi-1, "MIN"])/2))
        }else{
          Point9<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=BeginTrend[[j]][LastBi,"MAX"]-abs(BeginTrend[[j]][LastBi, "MAX"]-BeginTrend[[j]][LastBi, "MIN"])/2))
          Point10<-rbind(tempResult[[1]], data.frame(Ind=nrow(tempResult[[1]])+1, Price=BeginTrend[[j]][LastBi,"MIN"]-abs(BeginTrend[[j]][LastBi, "MAX"]-BeginTrend[[j]][LastBi, "MIN"])/2))
        }
      }
      
      Points<-list(Point1=Point1,Point2=Point2,Point3=Point3,Point4=Point4,Point5=Point5,
                   Point6=Point6,Point7=Point7,Point8=Point8,Point9=Point9,Point10=Point10)
      indices<-sapply(Points, function(x){is_empty(x)})
      Points<-Points[which(indices!=TRUE)]
      AllPoints[[paste0("Part",j)]]<-Points
    }
    return(AllPoints)
  }
  
  level_dim<-list(1)
  for(k in 1:n){#loop for n number of times. This will give n levels of data
    AllPoints<-GetPoints(BeginTrend) #get points for each dataframe in the BeginTrend list, for each k
    level_dim<-c(level_dim, list(as.numeric(lengths(AllPoints))))
    
    NewBeginTrend<-list()      
    SimResult[[paste0("Level",k+1)]]<-AllPoints #combine the existing data points with new iterated ones
    
    for(i in 1:length(AllPoints)){
      NewBeginTrend<-c(NewBeginTrend, lapply(AllPoints[[i]],function(x) PointsConverter(Point=x,Targetframe=BeginTrend[[i]])))
    }
    BeginTrend<-NewBeginTrend
  }
  
  
  #the following is to create a matrix for the plot layout and then plot the simulations
  if(SeparateSim==TRUE){ #this plot individual simulation in each plot
    LastLevelDim<-length(level_dim)
    DimPoints<-matrix(0, nrow=LastLevelDim, ncol = sum(level_dim[[LastLevelDim]]))
    DimPoints[LastLevelDim,]<-seq(to=sum(unlist(level_dim)), by=1, length.out=sum(level_dim[[LastLevelDim]]))
    
    for(i in LastLevelDim:2){
      if(DimPoints[i,1]-1>1){
        temp<-rep(seq(to=DimPoints[i,1]-1, by=1, length.out=length(level_dim[[i]])), times=level_dim[[i]])
        while(length(temp)<ncol(DimPoints)){
          if(i<=(LastLevelDim-1)){
            for(j in i:(LastLevelDim-1)){
              temp<-rep(temp, times=level_dim[[j+1]])   
            }
          }
        }
        DimPoints[i-1,]<-temp
      }
      else{
        DimPoints[1,]<-rep(1, times=sum(sum(level_dim[[LastLevelDim]])))
      }
    }
    
    #create the first and second level:
    FstScndLevel<-list() 
    for (i in 1:LastLevelDim){
      for (j in 1:length(level_dim[[i]])){
        FstScndLevel<-c(FstScndLevel, list(c(i,j)))
      }
    }   
    
    #create the sequence for the third level:
    permutations<-function(ListObj,Perm){
      if(length(ListObj)!=1){
        for(dim in 1:length(ListObj)){
          Perm<-permutations(ListObj[[dim]],Perm)
        }
      }else{
        Perm<-c(Perm,list(seq(1, length.out=ListObj)))
      }
      return(Perm)
    }
    ThirdLevel<-permutations(ListObj = level_dim, Perm=list())
    
    #combine the levels as a data frame:
    AllLevel<-data.frame()
    for (i in 1:length(FstScndLevel)){
      PlotMatrix<-apply(matrix(ThirdLevel[[i]], ncol=1), 1, function(x) c(FstScndLevel[[i]],x))
      PlotMatrix<-t(PlotMatrix)
      AllLevel<-rbind(AllLevel, PlotMatrix)  
    }
    
    #plot each row of the data frame
    if(n<=2){
      nf<-graphics::layout(DimPoints)
      PlotFrom<-2
      PlotTo<-nrow(AllLevel)
    }else{
      nf<-par(mfrow=c(1,1))
      cat("Plot range from: ",c(2,nrow(AllLevel)))
      PlotFrom<-as.numeric(readline(prompt = "Please choose the beginning plot: "))
      PlotTo<-as.numeric(readline(prompt = "Please choose the ending plot: "))
    }
    plot(SimResult[[1]], type="l",xlab="",ylab="")
    for (row in PlotFrom:PlotTo){
      plot(SimResult[[ as.numeric(AllLevel[row,]) ]], type="l",xlab="",ylab="")
    }
    par(mfrow=c(1,1)) #restore the layout
  }
  else if(CombineSim==TRUE){ #this plot all simulations in one plot
    test<-lapply(SimResult[[length(SimResult)]], function(x) reduce(x, inner_join, by="Ind"))
    test<-test%>%reduce(inner_join, by="Ind")
    colnames(test)<-c("Ind",paste("x",seq(from=1, to=ncol(test)-1), sep = ""))
    
    test<-gather(test, key="Trajectory", value="Value", colnames(test)[-1],factor_key = TRUE)
    
    ggplot(test, aes(Ind, Value, col=Trajectory))+geom_line()
    
  }
  else{return(SimResult)} #no plot, only return the data
}











