library(readxl)
VOIfn<-read.table("CMEVOI/RawData/Filename-all.txt", sep = "") #make sure in the Filename.txt there is a new blank space line after the final row, this way read.table won't give an error
VOIfn<-merge("CMEVOI/RawData/", VOIfn)
VOIfn<-cbind(paste(VOIfn[,1], VOIfn[,2], sep = ""), VOIfn[,2])

Instrument<-c("NQ")   #"NQ","CAD"

for (val in Instrument){
  VOI<-data.frame()
  
  for (i in 1:nrow(VOIfn)){
    if(str_sub(VOIfn[i,1], -4)=="xlsx"){
      Exfile <- read_xlsx(VOIfn[i,1], sheet = "CME Group Vol and OI by Product",.name_repair = "minimal")
    }else if(str_sub(VOIfn[i,1], -3)=="xls"){
      Exfile <- read_xls(VOIfn[i,1], sheet = "CME Group Vol and OI by Product",.name_repair = "minimal")
    }
    
    colnames(Exfile)<-paste0("V",c(1:ncol(Exfile)))
    Exfile <- Exfile %>%filter(rowSums(is.na(.)) < ncol(.))
    Exfile <- Exfile %>%select(where(~ !all(is.na(.))))
    colnames(Exfile)<-c("Description","Exchange","Commodity","ProductDescription","FutureOption","CMEGlobexVolume","PitVolume","ExPitVolume","OTCVolume","Volume","MTDADV","OpenInterest")
    Exfile<-Exfile[(which(Exfile$Description=="Description")+1):nrow(Exfile),] 
    Exfile<-Exfile%>%mutate(across(c("CMEGlobexVolume","PitVolume","ExPitVolume","OTCVolume","Volume","MTDADV","OpenInterest"), as.numeric))
    

    if (val=="NQ") {
      Exfile <- Exfile%>%filter(Commodity %in% c("NQ", "MNQ") & FutureOption=="F")%>%select(c(Volume,OpenInterest))%>%summarise(across(everything(),sum))%>%mutate(Date=ymd(parse_number(VOIfn[i,2])), .before=Volume)
    }
    
    VOI<-rbind(VOI,Exfile) #VOI data must have most recent data on the bottom, i.e. time ascending.
  }
  
  VOI$Date<-as.character(VOI$Date)
  
  if (val=="NQ"){
    write.csv(VOI,file=paste0(getwd(), "/CMEVOI/NQ_DailyVOI.csv"), row.names = F)
  }else if(val=="CAD"){
    write.csv(VOI,file=paste0(getwd(), "/CMEVOI/CAD_DailyVOI.csv"), row.names = F)
  }
}




CFTC_OIfn<-read.table("CFTC_OI/RawData/Filename.txt", sep = "") #make sure in the Filename.txt there is a new blank space line after the final row, this way read.table won't give an error
CFTC_OIfn<-merge("CFTC_OI/RawData/", CFTC_OIfn)
CFTC_OIfn<-cbind(paste(CFTC_OIfn[,1], CFTC_OIfn[,2], sep = ""), CFTC_OIfn[,2])
CFTC_OIfn
OIColumns<-c("Market_and_Exchange_Names",
             "Report_Date_as_MM_DD_YYYY",
             "Open_Interest_All",
             "NonComm_Positions_Long_All",
             "NonComm_Positions_Short_All",
             "NonComm_Postions_Spread_All",
             "Comm_Positions_Long_All",
             "Comm_Positions_Short_All",
             "Tot_Rept_Positions_Long_All",
             "Tot_Rept_Positions_Short_All",
             "NonRept_Positions_Long_All",
             "NonRept_Positions_Short_All",
             "Change_in_Open_Interest_All",
             "Change_in_NonComm_Long_All",
             "Change_in_NonComm_Short_All",
             "Change_in_NonComm_Spead_All",
             "Change_in_Comm_Long_All",
             "Change_in_Comm_Short_All",
             "Change_in_Tot_Rept_Long_All",
             "Change_in_Tot_Rept_Short_All",
             "Change_in_NonRept_Long_All",
             "Change_in_NonRept_Short_All",
             "Pct_of_Open_Interest_All",
             "Pct_of_OI_NonComm_Long_All",
             "Pct_of_OI_NonComm_Short_All",
             "Pct_of_OI_NonComm_Spread_All",
             "Pct_of_OI_Comm_Long_All",
             "Pct_of_OI_Comm_Short_All",
             "Pct_of_OI_Tot_Rept_Long_All",
             "Pct_of_OI_Tot_Rept_Short_All",
             "Pct_of_OI_NonRept_Long_All",
             "Pct_of_OI_NonRept_Short_All",
             "Traders_Tot_All",
             "Traders_NonComm_Long_All",
             "Traders_NonComm_Short_All",
             "Traders_NonComm_Spread_All",
             "Traders_Comm_Long_All",
             "Traders_Comm_Short_All",
             "Traders_Tot_Rept_Long_All",
             "Traders_Tot_Rept_Short_All")

for (val in Instrument){
  CFTC_OI<-data.frame()
  
  for (i in 1:nrow(CFTC_OIfn)){
    Exfile <-read_xlsx(path=CFTC_OIfn[i,1], col_names =TRUE)
    Exfile <- Exfile[,OIColumns]
    colnames(Exfile)[1:2]<-c("Description", "Date")
    Exfile$Date<-as.Date(Exfile$Date)
    
    if (val=="NQ") {      
      Exfile<-subset(Exfile,Description=="NASDAQ MINI - CHICAGO MERCANTILE EXCHANGE")
    }else if(val=="CAD"){
      Exfile<-subset(Exfile,Description=="CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE")
    }
    
    CFTC_OI<-rbind(Exfile,CFTC_OI) #CFTC_OI data must have most recent data on top, i.e. time descending.
  }
  
  if (val=="NQ"){
    CFTC_OI<-CFTC_OI[order(CFTC_OI$Date, decreasing=FALSE),] #resort by date ascending.
    write.csv(CFTC_OI,file=paste0(getwd(), "CFTC_OI/NQ_Open_Interest.csv"), row.names = F)
  }else if(val=="CAD"){
    CFTC_OI<-CFTC_OI[order(CFTC_OI$Date, decreasing=FALSE),]
    write.csv(CFTC_OI,file=paste0(getwd(), "CFTC_OI/CAD_Open_Interest.csv"), row.names = F)
  }
  
}

COT_na_approx<-function(pricedata, COT){
  x<-left_join(pricedata[,c(1,2)], COT[,2:ncol(COT)], by="Date")
  
  y<-na.approx(x[,-1], na.rm = FALSE, rule=2)
  y<-as.data.frame(y)
  y<-cbind(Date=x$Date,y[,-1])
  
  return(y)
}