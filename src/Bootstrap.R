PickModel<-function(PriceData, Test=FALSE){   #this gives the information of every complete planet, including data, class, slope etc.
  ModelInfo<-MACDTest(PriceData)
  if(Test==FALSE){
    if(ModelInfo[[length(ModelInfo)]][["Reversal"]]=="ReverseUnknown"){ModelInfo<-ModelInfo[-length(ModelInfo)]}
  }
  #ModelInfo<-lapply(AllSignals, function(x){y<-c(
  #list(Data=subset(PriceData, Date>=x[["Period"]][1])%>%subset(Date<=x[["Period"]][4])),
  # list(SLOPE=x[["BOLL信号"]][1]),
  #x); return(y)}
  #)
  return(ModelInfo)
}




CreateClassData<-function(ModelInfo, Test=FALSE){ #this creates the main class that contains the sub classes
  if (Test==FALSE){
    MainClassData<-sapply(ModelInfo, function(x){y<-list(x);names(y)<-make.names(x$Class);return(y) } ) #give each sublist the name according to its class
    ClassName<-c("ReverseTRUE","ReverseFALSE")
    MainClassData<-sapply(unique(ClassName), function(x){list(MainClassData[map_lgl(MainClassData, function(y){y["Reversal"]==x })]) }, USE.NAMES = TRUE) #regroup according to the same name
    MainClassData<-lapply(MainClassData, function(x){setNames(x, nm=make.names(names(x),unique = TRUE) )}) #make the sublists name unique 
  }else{
    MainClassData<-sapply(ModelInfo, function(x){y<-list(x);names(y)<-make.names(x$Class);return(y) } ) #give each sublist the name according to its class
    ClassName<-MainClassData[[1]]$Reversal
    MainClassData<-list(MainClassData)
    names(MainClassData)<-ClassName
  }
  
  return(MainClassData)
}




CreateSignal<-function(x){
  MACD<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["MACD"]]})); colnames(y)<-c("能量背驰","面积背驰","能量强度","面积强度"); return(y) })
  MFI<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["MFI"]]})); colnames(y)<-c("MFI背驰","MFI强度","量比"); return(y) })
  Str<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["形态背驰"]]})); colnames(y)<-c("形态背驰","形态强度"); return(y) })
  BOLL<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["BOLL信号"]]})); colnames(y)<-c("BOLL方向","BOLL强度"); return(y) })
  Candle<-lapply(x, function(x){y<-t(sapply(x, function(x){x[["Other"]]})); colnames(y)<-c("启动K线排名","分型强度"); return(y) })
  Result<-list(MACD=MACD,MFI=MFI,Str=Str,BOLL=BOLL,Candle=Candle) 
  return(Result)
}




ToSignal<-function(x,NumSignals){
  y<-strsplit(str_sub(x$Index, start = -NumSignals), "")#The digit part of the string "X1101101" of the Index name. Change it if MACDPower signals are changed. 
  y<-sapply(y, function(x){sum(as.numeric(x))} )
}




FitModel<-function(MainClassData,ClassData){
  ModelResult<-CreateSignal(MainClassData)
  DataResult<-CreateSignal(ClassData)
  
  #If MACDPower signals are changed, make sure to change the indices below for the part "-c(...)"
  FitMACD<-map(ModelResult$MACD, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1:2)]-DataResult$MACD[[1]][,-c(1:2)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitMFI<-map(ModelResult$MFI, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$MFI[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitStr<-map(ModelResult$Str, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$Str[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitBOLL<-map(ModelResult$BOLL, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$BOLL[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  FitCandle<-map(ModelResult$Candle, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$Candle[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
  Fit<-tibble(FitMACD=FitMACD,FitMFI=FitMFI,FitStr=FitStr,FitBOLL=FitBOLL,FitCandle=FitCandle) 
  
  PowerF<-function(x,st){
    PowerTable<-data.frame(Value=x$ReverseTRUE)%>%mutate(Index=str_sub(rownames(.), start = -(st+1)),Rank=rank(Value, ties.method = "random"))%>%mutate(Signal=ToSignal(x=., NumSignals=st), Power="ReverseTRUE")%>%arrange(desc(Signal),Rank)
    ErrorTable<-data.frame(Value=x$ReverseFALSE)%>%mutate(Index=str_sub(rownames(.), start = -(st+1)),Rank=rank(Value, ties.method = "random"))%>%mutate(Signal=ToSignal(x=., NumSignals=st), Power="ReverseFALSE")%>%arrange(desc(Signal),Rank)

    Pvalue<-(ErrorTable %>% filter(Signal>=ErrorTable[which(ErrorTable$Rank==1),"Signal"]) %>%nrow())/nrow(ErrorTable)
    SampleTypeII_error<-(PowerTable %>% filter(Signal<PowerTable[which(PowerTable$Rank==1),"Signal"]) %>%nrow())/nrow(PowerTable)
    SamplePower<-1-SampleTypeII_error
    
    TrueTypeI_error<-(ErrorTable%>%filter(Signal>=5)%>%nrow())/nrow(ErrorTable)
    TrueTypeII_error<-(PowerTable%>%filter(Signal<5)%>%nrow())/nrow(PowerTable)
    TruePower<-1-TrueTypeII_error
    
    y<-data.frame(Pvalue, SamplePower, TrueTypeI_error, TruePower)
    return(y)
  }
  
  NumSignals=length(c("能量背驰","面积背驰","MFI背驰","形态背驰","BOLL强度","启动K线排名","分型强度"))
  FitResult<-map(Fit,~PowerF(., st = NumSignals))%>%do.call(rbind,.)%>%mutate(Signal=rownames(.),.before=1)
  return(FitResult)
}



#The OriginalData must be included in a named list like list=(NQ=NQ)
MainBootstrap<-function(DataToBeFit,OriginalData, ModelInfo=NULL){ #create results and combine and average them
  if(is.null(ModelInfo)==TRUE){ModelInfo<-do.call(c,lapply(OriginalData, function(x) PickModel(x)))} #contains 1-level sublist of all planets
  DataInfo<-PickModel(DataToBeFit, Test = TRUE) #should contain only 1-level sublist of one planet, should have the same structure as ModelInfo
  DataInfo<-DataInfo[length(DataInfo)] #make sure there's only the most recent one to be tested
  
  ModelInfoSlope<-sapply(1:length(ModelInfo), function(i) (ModelInfo[[i]]$SLOPE))
  DataInfoSlope<-as.vector(DataInfo[[1]]$SLOPE)
  
  ModelID<-which(ModelInfoSlope %in% DataInfoSlope)
  if (length(ModelID)==0){
    stop("There is no planet model for the data to be tested! Try with a new original data.")
  }
  SelectedModel<-lapply(ModelID, function(x) (ModelInfo[[x]]))
  names(SelectedModel)<-names(ModelInfo)[ModelID]
  
  MainClassData<-CreateClassData(SelectedModel) #simulate nboot number of data for each SelectedModel
  ClassData<-CreateClassData(DataInfo, Test = TRUE)
  GlobalResult<-FitModel(MainClassData=MainClassData,ClassData=ClassData)
  return(GlobalResult) #GlobalResult gives the overall averaged result within each Class
}


ReversalPlot<-function(Pricedata, MainClassData, ReversalType, Index){
  Data_macd<-PricedataMACD(Pricedata) #calculate the MACD
  Data_mfi<-PricedataMFI(Pricedata) #calculate the MFI
  SignalData<-list(Data_macd=Data_macd,Data_mfi=Data_mfi)
  
  D<-MainClassData[[ReversalType]][[Index]][["Period"]]
  SignalPlot(list(Data=subset(Pricedata, Date>=D$In1 & Date<=D$Out2)),AddSignal=TRUE,SignalData=SignalData)
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



Hypothesis<-function(ModelInfo=NULL,Data=NULL){ #If using Data, it must be a list, e.g. list(QQQ_daily=QQQ_daily, QQQ_weekly=QQQ_weekly)
  if(is.null(ModelInfo)==TRUE){
    ModelInfo<-do.call(c,lapply(Data, function(x) PickModel(x)))
    saveRDS(ModelInfo,file = "Data/RData/ModelInfo.rds")
  }
  
  MainClassData<-CreateClassData(ModelInfo)
  
  
  ModelResult<-CreateSignal(MainClassData)
  NumSignals=length(c("能量背驰","面积背驰","MFI背驰","形态背驰","BOLL强度","启动K线排名","分型强度"))
  
  PowerTable<-data.frame(ModelResult$MACD$ReverseTRUE)%>%transmute(Index=rownames(.))%>%mutate(Signal=ToSignal(., NumSignals = NumSignals), Power="ReverseTRUE")%>%arrange(desc(Signal))
  ErrorTable<-data.frame(ModelResult$MACD$ReverseFALSE)%>%transmute(Index=rownames(.))%>%mutate(Signal=ToSignal(., NumSignals = NumSignals), Power="ReverseTRUE")%>%arrange(desc(Signal))
  
  DecisionTable<-do.call(rbind,lapply(1:NumSignals, function(sig){
    correct<-PowerTable%>%filter(Signal>=sig)%>%nrow()
    incorrect<-ErrorTable%>%filter(Signal>=sig)%>%nrow()
    
    Power<-correct/nrow(PowerTable) #If reverse true, how many scenarios I can catch. 1-Power will be how many scenarios I'll miss
    Alpha<-incorrect/nrow(ErrorTable) #If reverse false, how many false signal. More dangerous
    SuccessRate<-correct/(correct+incorrect) #Given a signal (rejection region), one needs to reject if >= this signal. But it can either be correct or incorrect. 
    #This SuccessRate is the number of scenarios in ReverseTrue/ number of scenarios in ReverseFalse 
    
    return(data.frame(Alpha,Power,SuccessRate))
  }))%>%mutate(RejectionRegion=1:NumSignals,.before=Alpha)%>%mutate(Diff=Power-Alpha,.before=SuccessRate)%>%arrange(desc(Alpha)) 
  #Given alpha level, choose the max of the Diff is equivalent as minimizing alpha+beta.
  #Smaller alpha can give higher success rate, but also very small power, so will miss most of the chances.
  
  print(DecisionTable)
  AlphaPower<-data.frame(RejectionRegion=1:NumSignals, Alpha=DecisionTable$Alpha, Power=DecisionTable$Power)%>%gather("Type","Value",-1)
  ggplot(AlphaPower, aes(RejectionRegion,Value,color=Type))+geom_line()
}





SimTrend<-function(Data,n,SeparateSim=FALSE,CombineSim=FALSE){
  if(n>4){stop("Cannot plot more than four levels!")}
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












