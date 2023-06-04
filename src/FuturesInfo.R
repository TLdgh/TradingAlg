GetFutInfo<-function(FUT,interval){
  BasicInfo<-merge(FUT,interval)
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
                                   endDateTime=format(with_tz(Sys.time(),tz="Canada/Eastern"),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20230616",currency="USD",multiplier="20")}
    else if(BasicInfo[i,1]=="GC"){
      FutInfo[[paste0("GC",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("GC"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                   endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="COMEX",expiry="20211229",currency="USD",multiplier="100")}
    else if(BasicInfo[i,1]=="CAD"){
      FutInfo[[paste0("CAD",BasicInfo[i,2])]] <- cbind(SecurityType="FUT",Symb=c("CAD"),intv=BasicInfo[i,2],barSize=barsize,duration=duration,
                                    endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="CME",expiry="20220315",currency="USD",multiplier="100000")}
  }
  return(FutInfo)
}



