ModelInfo<-PickModel(NQ5FContinuous)
NewM<-PickModel(NQ4HContinuous)
ModelInfo<-c(ModelInfo, NewM)


DataInfo<-PickModel(test, Test = TRUE) #should contain only 1-level sublist of one planet, should have the same structure as ModelInfo
DataInfo<-DataInfo[length(DataInfo)] #make sure there's only the most recent one to be tested
ModelInfoSlope<-sapply(1:length(ModelInfo), function(i) (ModelInfo[[i]]$SLOPE))
DataInfoSlope<-as.vector(DataInfo[[1]]$SLOPE)
ModelID<-which(ModelInfoSlope %in% DataInfoSlope)
if (length(ModelID)==0){
  stop("There is no planet model for the data to be tested! Try with a new original data.")
}
SelectedModel<-lapply(ModelID, function(x) (ModelInfo[[x]]))
MainClassData<-CreateClassData(SelectedModel) #simulate nboot number of data for each SelectedModel
ClassData<-CreateClassData(DataInfo, Test = TRUE)
ModelResult<-CreateSignal(MainClassData)
DataResult<-CreateSignal(ClassData)
FitMACD<-map(ModelResult$MACD, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1:3)]-DataResult$MACD[[1]][,-c(1:3)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
FitMFI<-map(ModelResult$MFI, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$MFI[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
FitStr<-map(ModelResult$Str, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$Str[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
FitBOLL<-map(ModelResult$BOLL, function(x){apply(x,MARGIN = 1 ,function(x){y<-(x[-c(1)]-DataResult$BOLL[[1]][,-c(1)]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
FitCandle<-map(ModelResult$Candle, function(x){apply(x,MARGIN = 1 ,function(x){y<-t(x-DataResult$Candle[[1]]); y<-t(y)%*%y; return(y) })}) #sum of squares using inner product
Fit<-tibble(FitMACD=FitMACD,FitMFI=FitMFI,FitStr=FitStr,FitBOLL=FitBOLL,FitCandle=FitCandle) 

PowerTable<-data.frame(Value=FitCandle$ReverseTRUE)%>%mutate(Index=rownames(.),Rank=rank(Value, ties.method = "random"))%>%mutate(Signal=ToSignal(.), Power="ReverseTRUE")%>%arrange(desc(Signal),Rank)
ErrorTable<-data.frame(Value=FitCandle$ReverseFALSE)%>%mutate(Index=rownames(.),Rank=rank(Value, ties.method = "random"))%>%mutate(Signal=ToSignal(.), Power="ReverseFALSE")%>%arrange(desc(Signal),Rank)

nrow(filter(PowerTable, Signal>=5))/nrow(PowerTable)#Given true reversal, the probability of detecting reversal (success rate or power, doesn't loose money if the signal is false, just missed the reversal) 
nrow(filter(ErrorTable, Signal>=5))/nrow(ErrorTable)#Given false reversal, the probability of detecting reversal (type I error, will loose money if the signal is false, more dangerous) 

ReversalPlot(NQ5FContinuous, MainClassData = MainClassData, ReversalType = "ReverseTRUE", 1)