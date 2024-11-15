tws <- twsConnect(port = 7496) #to connect with TWS
isConnected(tws)#check

# Modify the expiry and currency as needed for your specific contract
contract <- twsFuture(
  symbol = "NQ",      # Symbol for NASDAQ 100 Futures
  exch = "CME",    # Exchange for NQ Futures
  expiry = "20241220",  # Expiry date in YYYYMM format
  currency = "USD",
  multiplier="20"
)

customWrapper<-function(n){
  eW <- eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 
                                                            3), ncol = 3), 0), 
                                            .Dimnames = list(NULL, c("Last","LastSize","Volume")))), n))
                                            
  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    
    if (tickType == .twsTickType$LAST) {
      cat(paste(timestamp, msg[4], msg[5], "", sep = ","), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 1:2] <- msg[4:5]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
    data <- eW$get.Data("data")
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    file <- file[[id]]
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    
    if (tickType == .twsTickType$VOLUME) {
      cat(paste(timestamp, "", "", msg[4], sep = ","), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 3] <- msg[4]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}



fh<-file('TickDistri_NQ.csv',open="a")
reqMktData(tws,contract, eventWrapper = customWrapper(1), file = fh)
# Alternatively, this gives the bars of every 5sec: reqRealTimeBars(tws,contract,useRTH = FALSE,eventWrapper=eWrapper.RealTimeBars.CSV(1),file=fh)
close(fh)



x=read.csv("TickDistri_NQ.csv", header = FALSE)
colnames(x)=c("Time","Last","LastSize","Volume")
y=x%>%filter(!if_all(c(Last,Volume), is.na))
y[1,"Volume"]=first(na.omit(y$Volume))-1
y=y%>%mutate(Volume=if_else(is.na(Volume), lag(Volume)+1, Volume))%>%na.omit()%>%
  mutate(LastSize=if_else(row_number()>1 & (Volume-LastSize)>lag(Volume), Volume-lag(Volume), LastSize))

y%>%plot_ly(x=~LastSize,y=~Last,type="bar", orientation="h")




