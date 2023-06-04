GetStockInfo<-function(STK,interval,GlobalMarket="US", A_STOK=NULL){
  BasicInfo<-merge(STK,interval)
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
                                                                                             endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), GlobalMarket=GlobalMarket, A_STOK=A_STOK)
    }
  }
  
  
  return(StkInfo)
}



