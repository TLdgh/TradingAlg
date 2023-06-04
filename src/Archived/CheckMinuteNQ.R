NQ1Finfo <- cbind(SecurityType="FUT", Symb=c("NQ"),intv=c("1F"),barSize=c("1 min"), duration=c("1 D"),
                    endDateTime=format(Sys.time(), "%Y%m%d %H:%M:%S"), exch="GLOBEX", expiry="20211217",currency="USD",multiplier="20")
FutToBePrepared <-NQ1Finfo   # rbind(NQ30Sinfo,NQinfo)     rbind(GCinfo,NQinfo) 
source("/Users/tengli/R/Script/PrepFutures.R")  #This script downloads the data and prepare for combination
source("/Users/tengli/R/Script/SignalPlot.R") #everytime we run a function from a different script, we must run this command
  SignalPlot(NQ1F,Title = "NQ1F")
