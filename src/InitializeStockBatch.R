RE<-c("PLD","AMT","EQIX","WELL","O","PSA","SPG","DLR","CCI","CBRE","IRM",
      "EXR","VICI","CSGP","AVB","VTR","SBAC","EQR","WY","INVH","ESS","ARE",
      "MAA","KIM","DOC","UDR","BXP","CPT","HST","REG","FRT")


Technology<-c("AAPL","NVDA","MSFT","AVGO","CRM","ORCL","AMD","ACN","CSCO","ADBE",
              "IBM","QCOM","NOW","TXN","INTU","AMAT","MU","PANW","ADI","ANET","INTC",
              "LRCX","KLAC","PLTR","APH","MSI","SNPS","CRWD","CDNS","ADSK","ROP","NXPI",
              "FTNT","FICO","TEL","MPWR","MCHP","IT","CTSH","HPQ","DELL","GLW","ON","CDW",
              "ANSS","KEYS","HPE","NTAP","TYL","SMCI","STX","GDDY","PTC","WDC","FSLR","TDY",
              "TER","ZBRA","AKAM","VRSN","SWKS","TRMB","GEN","JBL","JNPR","FFIV","ENPH","EPAM","QRVO")


Utilities<-c("NEE","SO","DUK","CEG","SRE","AEP","D","VST","PEG","PCG","EXC","ED","XEL",
             "EIX","WEC","ETR","AWK","DTE","PPL","AEE","ES","FE","ATO","CMS","CNP","NRG",
             "LNT","NI","EVRG","AES","PNW")


Materials<-c("LIN","SHW","APD","FCX","ECL","NEM","CTVA","NUE","DOW","DD","MLM","VMC","PPG",
             "IFF","LYB","SW","BALL","PKG","STLD","AVY","IP","AMCR","CF","CE","EMN","ALB","MOS","FMC")


Industrials<-c("GE","CAT","RTX","UBER","UNP","HON","ETN","LMT","ADP","DE","UPS","TT","BA",
               "PH","GD","TDG","WM","GEV","MMM","CTAS","NOC","ITW","CARR","CSX","EMR","FDX",
               "PCAR","NSC","URI","JCI","GWW","LHX","CPRT","PWR","CMI","PAYX","FAST","HWM",
               "OTIS","RSG","IR","AME","VRSK","ODFL","DAL","EFX","WAB","XYL","AXON","ROK",
               "VLTO","FTV","DOV","BR","HUBB","UAL","LDOS","BLDR","MAS","LUV","J","SNA",
               "EXPD","TXT","PNR","SWK","IEX","JBHT","ROL","NDSN","ALLE","CHRW","DAY","HII",
               "GNRC","AOS","PAYC","AMTM")


Healthcare<-c("LLY","UNH","JNJ","ABBV","MRK","TMO","ABT","ISRG","DHR","AMGN","PFE","BSX",
              "SYK","VRTX","MDT","GILD","BMY","REGN","ELV","CI","ZTS","HCA","CVS","BDX",
              "MCK","EW","GEHC","COR","IQV","A","IDXX","RMD","CNC","HUM","MTD","DXCM","BIIB",
              "CAH","STE","ZBH","COO","WST","WAT","HOLX","BAX","MRNA","LH","MOH","DGX","PODD",
              "ALGN","RVTY","UHS","VTRS","TFX","TECH","CTLT","INCY","CRL","SOLV","HSIC","DVA")


Financials<-c("BRK B","JPM","V","MA","BAC","WFC","SPGI","GS","AXP","MS","PGR","BLK","BX","C",
              "FI","CB","MMC","SCHW","ICE","KKR","PYPL","CME","AON","USB","MCO","PNC","AJG",
              "COF","TRV","TFC","AFL","BK","ALL","AMP","AIG","MET","FIS","MSCI","PRU","ACGL",
              "DFS","HIG","MTB","NDAQ","FITB","WTW","STT","GPN","TROW","RJF","BRO","CPAY",
              "HBAN","SYF","CINF","CBOE","RF","NTRS","PFG","CFG","WRB","FDS","EG","KEY","L",
              "JKHY","ERIE","MKTX","AIZ","GL","IVZ","BEN")


Energy<-c("XOM","CVX","COP","WMB","EOG","SLB","OKE","PSX","MPC","KMI","VLO","HES",
          "BKR","TRGP","OXY","FANG","DVN","HAL","EQT","CTRA","MRO","APA")


ConsumerStaples<-c("PG","COST","WMT","KO","PEP","PM","MDLZ","CL","MO","TGT","KMB",
                   "KVUE","KDP","GIS","MNST","STZ","KR","SYY","KHC","HSY","ADM","CHD",
                   "K","EL","MKC","CLX","DG","TSN","CAG","DLTR","SJM","BG","LW","TAP",
                   "CPB","HRL","BF B","WBA")


ConsumerDiscre<-c("AMZN","TSLA","HD","MCD","LOW","BKNG","TJX","SBUX","NKE","CMG","ORLY",
                  "MAR","ABNB","HLT","DHI","GM","AZO","ROST","RCL","LEN","F","YUM","LULU",
                  "TSCO","EBAY","PHM","NVR","GRMN","DECK","CCL","GPC","EXPE","DRI","APTV",
                  "BBY","LVS","ULTA","DPZ","POOL","KMX","NCLH","TPR","LKQ","CZR","HAS",
                  "MGM","WYNN","MHK","RL","BWA")


CommunicationSvcs<-c("META","GOOGL","NFLX","TMUS","DIS","CMCSA","T","TTWO",
                     "VZ","EA","CHTR","OMC","LYV","WBD","IPG","NWSA","MTCH","FOXA","PARA","FOX","NWS")



All<-c(RE,Technology,Utilities,Materials,Industrials,Healthcare,Financials,Energy,ConsumerStaples,ConsumerDiscre,CommunicationSvcs)

for(stock in All){
  tryCatch(
    expr={
      file_path=paste0(getwd(),"/Data/OriginalStockData/US/", stock,"_daily.csv")
      NDurations<-1 
      Sdata<-list()
      endDateTime<-format(Sys.time(),"%Y%m%d %H:%M:%S")
      
      for(i in 1:NDurations){
        Contract<-twsEquity(symbol=stock, exch = "SMART", currency = "USD")
        SdataNew<-reqHistoricalData(conn=tws, Contract=Contract, endDateTime=endDateTime, barSize="1 day", duration="1 Y", useRTH='1', whatToShow='TRADES') 
        Sdata[[NDurations+1-i]]<-SdataNew
        endDateTime<-format(as.POSIXct(index(SdataNew[1,]),tz="America/Toronto"),"%Y%m%d %H:%M:%S")
        Sys.sleep(round(22*runif(1,min = 1, max = 1.2)))
      }  
      
      Sdata<-do.call(rbind, Sdata)
      write.zoo(Sdata, sep=",", file=file_path)
      cat("stock ", stock, "is finished.", "\n")
      
    },
    warning=function(w){
      # Handling the warning: Skip this iteration
      warning_message <- paste("Warning for stock", stock, ":", w$message, "\n")
      writeLines(warning_message, con = file("logs/InitializeStock_log.txt", open = "a"))  # Open in append mode
    }
  )
  next # Skip to the next iteration
}
close(file("logs/InitializeStock_log.txt", open = "a"))





