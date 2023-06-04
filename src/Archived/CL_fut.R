FutToBePrepared<-cbind(SecurityType="FUT",Symb=c("CL"),intv="BasicInfo[i,2]",barSize="barsize",duration="duration",
      endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"),exch="GLOBEX",expiry="20211217",currency="USD",multiplier="20")}


Contract<-twsFuture(symbol="CL",exch="NYMEX", expiry="20221220", currency="USD", multiplier=1000, include_expired="1")
Fdata<-reqHistoricalData(tws, Contract=Contract, endDateTime=format(Sys.time(),"%Y%m%d %H:%M:%S"), barSize="30 mins", duration="1 M", useRTH='0', whatToShow='TRADES') 
