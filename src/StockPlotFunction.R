PriceChart<-function(Pricedata, Title){
  StarData <- StarFunction(Pricedata)
  Bi<-BiFunction(StarData)
  Finalplanet <- as.data.frame(PlanetFunction(Bi))
  Finalplanet <- subset(Finalplanet, PlanetHigh!=0)
  LSegment<-LineSegment(Bi)
  Pricedata_EMA10 <- FuncEMA10(Pricedata)
  Pricedata_EMA30 <- FuncEMA30(Pricedata)
  Pricedata_EMA60 <- FuncEMA60(Pricedata)
  Pricedata_BOLL<-PricedataBOLL(Pricedata)
  Pricedata_SAR<-PricedataSAR(Pricedata)
  
  shape <- list( #initiate a rectangular layout shape object, see detail in https://plotly.com/r/horizontal-vertical-shapes/
    type = "rect",
    fillcolor="pink",
    opacity = 0.3,
    line = list(color = "black",width=2),
    xref = "x",
    yref = "y"
  )
  
  lines <- list()
  for (i in 1:nrow(Finalplanet)) {
    shape[["x0"]] <- Finalplanet[i,]$PlanetStartD
    shape[["x1"]] <- Finalplanet[i,]$PlanetEndD
    shape[["y0"]] <- Finalplanet[i,]$PlanetLow
    shape[["y1"]] <- Finalplanet[i,]$PlanetHigh
    lines <- c(lines, list(shape))
  }
  
  annote <- list( #initiate an annotation object for the final planet, see detail in https://plotly.com/r/horizontal-vertical-shapes/
    xref = "x",
    yref = "y"
  )
  
  arrows <- list()
  for (j in 3:4) {
    for (i in 1:nrow(Finalplanet)) {
      annote[["x"]] <- Finalplanet[i,j] #PlanetStartD
      annote[["y"]] <- Finalplanet[i,j-2] #PlanetHigh
      annote[["text"]] <- Finalplanet[i,j-2] #PlanetHigh
      arrows <- c(arrows, list(annote))
    }
  }
  
  
  if(nrow(Finalplanet)==0){
    plot_ly(data=Pricedata, x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
      layout(xaxis = list(rangeslider = list(visible = F)))%>%
      add_lines(x=StarData$Date, y=StarData$Price, name='Bi',type='scatter', mode = 'lines',
                line=list(color='ivory', dash="dash", width=4), inherit = F)%>%
      add_lines(x=LSegment$Date, y=LSegment$Price, name='LineSegment',type='scatter', mode = 'lines',
                line=list(color='black', width=2), inherit = F)%>%
      add_lines(x=Pricedata_EMA10$Date, y=Pricedata_EMA10$EMA10, name='EMA10', type='scatter', mode='lines',
                line=list(color='#FF8C00', width=2),inherit = F)%>%
      add_lines(x=Pricedata_EMA30$Date, y=Pricedata_EMA30$EMA30, name='EMA30', type='scatter', mode='lines',
                line=list(color='#4169E1', width=2),inherit = F)%>%
      add_lines(x=Pricedata_EMA60$Date, y=Pricedata_EMA60$EMA60, name='EMA60', type='scatter', mode='lines',
                line=list(color='purple', width=2),inherit = F)%>%
      add_lines(x=Pricedata_BOLL$Date, y=Pricedata_BOLL$lwB, name='BOLL_lwB', type='scatter', mode='lines',
                line=list(color='silver', width=2),inherit = F)%>%
      add_lines(x=Pricedata_BOLL$Date, y=Pricedata_BOLL$upB, name='BOLL_upB', type='scatter', mode='lines',
                line=list(color='silver', width=2),inherit = F)%>%
      add_trace(x=Pricedata_SAR$Date, y=Pricedata_SAR$sar, name='SAR', type='scatter', mode='markers',
                marker = list(color = Pricedata_SAR$SAR_col, size=4),inherit = F)
  }else{
    plot_ly(data=Pricedata, x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
      layout(shapes=lines,annotations=arrows,xaxis = list(rangeslider = list(visible = F)))%>%
      add_lines(x=StarData$Date, y=StarData$Price, name='Bi',type='scatter', mode = 'lines',
                line=list(color='ivory', dash="dash", width=4), inherit = F)%>%
      add_lines(x=LSegment$Date, y=LSegment$Price, name='LineSegment',type='scatter', mode = 'lines',
                line=list(color='black', width=2), inherit = F)%>%
      add_lines(x=Pricedata_EMA10$Date, y=Pricedata_EMA10$EMA10, name='EMA10',type='scatter', mode='lines',
                line=list(color='#FF8C00', width=2),inherit = F)%>%
      add_lines(x=Pricedata_EMA30$Date, y=Pricedata_EMA30$EMA30, name='EMA30', type='scatter', mode='lines',
                line=list(color='#4169E1', width=2),inherit = F)%>%
      add_lines(x=Pricedata_EMA60$Date, y=Pricedata_EMA60$EMA60, name='EMA60', type='scatter', mode='lines',
                line=list(color='purple', width=2),inherit = F)%>%
      add_lines(x=Pricedata_BOLL$Date, y=Pricedata_BOLL$lwB, name='BOLL_lwB', type='scatter', mode='lines',
                line=list(color='silver', width=2),inherit = F)%>%
      add_lines(x=Pricedata_BOLL$Date, y=Pricedata_BOLL$upB, name='BOLL_upB', type='scatter', mode='lines',
                line=list(color='silver', width=2),inherit = F)%>%
      add_trace(x=Pricedata_SAR$Date, y=Pricedata_SAR$sar, name='SAR', type='scatter', mode='markers',
                marker = list(color = Pricedata_SAR$SAR_col, size=4),inherit = F)
  }
}


PricedataMACD <- function (Pricedata){
  Pricedata_macd <- as.data.frame(MACD(Cl(Pricedata[order(Pricedata$Date, decreasing = F),]), nFast = 5, nSlow = 20, nSig = 10, maType = EMA, percent = FALSE))
  Pricedata_macd$MACD <- (Pricedata_macd$macd - Pricedata_macd$signal)*2
  Pricedata_macd$Date <- Pricedata[order(Pricedata$Date, decreasing = F),]$Date
  colnames(Pricedata_macd) <- c("DIFF", "DEA", "MACD", "Date")
  return(Pricedata_macd)
}

MyMFI <-function(HLC, volume, n=14){
  
  # Money Flow Index
  
  HLC <- try.xts(HLC, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)
  
  if(!(is.xts(HLC) && is.xts(volume))) {
    HLC <- as.matrix(HLC)
    volume <- as.matrix(volume)
  }
  
  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      HLC <- xts(apply(HLC, 1, mean),index(HLC))
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else
    if(NCOL(HLC)!=1) {
      stop("Price series must be either High-Low-Close, or Close/univariate.")
    }
  
  if(is.xts(HLC)) {
    priceLag <- lag.xts(HLC)
  } else {
    priceLag <- c( NA, HLC[-NROW(HLC)] )
  }
  
  # Calculate Money Flow
  # Calculate positive and negative Money Flow
  pmf <- ifelse( HLC > priceLag, abs(HLC-priceLag)*volume, 0 )
  nmf <- ifelse( HLC < priceLag, abs(HLC-priceLag)*volume, 0 )
  
  # Calculate Money Ratio and Money Flow Index
  num <- runSum( pmf, n )
  den <- runSum( nmf, n )
  mr <- num / den
  mfi <- 100 - ( 100 / ( 1 + mr ) )
  mfi[0 == den] <- 100
  mfi[0 == den & 0 == num] <- 50
  
  if(is.xts(mfi)) colnames(mfi) <- 'mfi'
  
  reclass( mfi, HLC )
}

PricedataMoneyFlow<-function(Pricedata){
  x<-transmute(Pricedata, MF=(Close-lag(Close))*Volume)
  MF<-na.omit(data.frame(Date=Pricedata$Date,MoneyFlow=x$MF))
  return(MF)
}

PricedataMFI<-function(Pricedata){
  MFI<-MyMFI(HLC(Pricedata), volume = Pricedata$Volume, n=10)-50
  MFI<-na.omit(data.frame(Date=Pricedata$Date,MFI=MFI))
  return(MFI)
}

PricedataBOLL<-function(Pricedata){
  BOLL<- BBands(Cl(Pricedata), n=60, sd=2, maType = EMA)
  BOLL<-na.omit(data.frame(Date=Pricedata$Date,PctB=BOLL[,"pctB"], lwB=BOLL[,"dn"], upB=BOLL[,"up"]))
  return(BOLL)
}

PricedataSAR<-function(Pricedata){
  SAR_Pricedata<- Pricedata%>%transmute(Date=Pricedata$Date, sar=as.numeric(SAR(Pricedata[,c("High", "Low")], accel=c(0.01, 0.2))), SAR_col=ifelse(sar>=High, "red", "green"))
  SAR_Pricedata<-na.omit(data.frame(Date=Pricedata$Date,SAR_Pricedata))
  return(SAR_Pricedata)
}

FuncEMA60<-function(Pricedata){
  EMA60<-EMA(Cl(Pricedata[order(Pricedata$Date, decreasing = F),]), n=60)
  EMA60<-na.omit(data.frame(Date=Pricedata$Date,EMA60=EMA60))
  return(EMA60)
}

FuncEMA30<-function(Pricedata){
  EMA30<-EMA(Cl(Pricedata[order(Pricedata$Date, decreasing = F),]), n=30)
  EMA30<-na.omit(data.frame(Date=Pricedata$Date,EMA30=EMA30))
  return(EMA30)
}

FuncEMA10<-function(Pricedata){
  EMA10<-EMA(Cl(Pricedata[order(Pricedata$Date, decreasing = F),]), n=10)
  EMA10<-na.omit(data.frame(Date=Pricedata$Date,EMA10=EMA10))
  return(EMA10)
}

StockChart<-function (Pricedata, Title){
  Pricedata_macd <- PricedataMACD(Pricedata)
  Pricedata_MFI<-PricedataMFI(Pricedata)
  Pricedata_MoneyFlow<-PricedataMoneyFlow(Pricedata)
  Alldata<- merge(Pricedata,Pricedata_macd, by="Date")
  Alldata<- merge(Alldata,Pricedata_MFI, by="Date")
  Alldata<- merge(Alldata,Pricedata_MoneyFlow, by="Date")
  
  Alldata<-na.omit(Alldata)
  
  if(Alldata$MACD[1]>=0){Alldata$MACDdirection[1]<-"green"}else{Alldata$MACDdirection[1] = "red"}
  if(Alldata$MFI[1]>=0){Alldata$MFIdirection[1]<-"green"}else{Alldata$MFIdirection[1] = "red"}
  if(Alldata$Close[1] >= Alldata$Open[1]){Alldata$VOLdirection[1] = 'green'}else{Alldata$VOLdirection[1] = 'red'}
  
  for (i in 2:nrow(Alldata)) {       ##Color column for MACD, VMACD, MFI and Volume direction
    if(abs(Alldata$MACD[i])/abs(Alldata$MACD[i-1])<=0.5){Alldata$MACDdirection[i]<-"#8A2BE2"}
    else if(Alldata$MACD[i] >= 0){
      if(Alldata$MACD[i]>Alldata$MACD[i-1]){Alldata$MACDdirection[i]<-"green"}
      else{Alldata$MACDdirection[i]<-"palegreen"}}
    else if(Alldata$MACD[i]<0){
      if(Alldata$MACD[i]<Alldata$MACD[i-1]){Alldata$MACDdirection[i]<-"red"}else{Alldata$MACDdirection[i]<-"lightpink"}
    }
    
    if(abs(Alldata$MFI[i])/abs(Alldata$MFI[i-1])<=0.5){Alldata$MFIdirection[i]<-"#8A2BE2"}
    else if (Alldata$MFI[i] >= 0){
      if(Alldata$MFI[i]>Alldata$MFI[i-1]){Alldata$MFIdirection[i]<-"green"}else{Alldata$MFIdirection[i]<-"palegreen"}
    }else if(Alldata$MFI[i]<0){
      if(Alldata$MFI[i]<Alldata$MFI[i-1]){Alldata$MFIdirection[i]<-"red"}else{Alldata$MFIdirection[i]<-"lightpink"}
    }
    
    if (Alldata$Volume[i]<2*Alldata$Volume[i-1]){
      if(Alldata$Close[i] >= Alldata$Close[i-1]){Alldata$VOLdirection[i] = 'green'}
      else{Alldata$VOLdirection[i] = 'red'}
    }
    else{Alldata$VOLdirection[i] = 'gold'}
    
  }
  
  
  MoneyFlowChart<-plot_ly(data=Alldata, x=~Date, y=~MoneyFlow, type='bar', marker=list(color = ~VOLdirection))%>%
    layout(xaxis=list(showticklabels=FALSE))
  
  MACDChart<-plot_ly(data=Alldata, x=~Date)%>%
    add_trace(y=~DIFF,name="DIFF",type="scatter",mode = 'lines', line=list(color="orange", width=2))%>%
    add_trace(y=~DEA,name="DEA",type="scatter",mode = 'lines', line=list(color="blue", width=2))%>%
    add_bars(y=~MACD,name="MACD", marker=list(color=~MACDdirection))%>%
    layout(xaxis = list(rangeslider = list(visible = F)))
  
  MFIlines<-function(y, color="black"){list(type="line",x0=0, x1=1, y0=y,y1=y, xref="paper", line=list(color=color))} #this gives the lines in MFI chart
  MFIChart<-plot_ly(data=Alldata, x=~Date)%>%
    add_bars(y=~MFI,name="MFI", marker=list(color=~MFIdirection))%>%
    layout(xaxis = list(rangeslider = list(visible = F)), shapes=list(MFIlines(y=34), MFIlines(y=-34)))
  
  p<-subplot(PriceChart(Alldata, Title), MACDChart, MoneyFlowChart, MFIChart, nrows=4, shareX=TRUE, heights = c(0.4, 0.15, 0.3, 0.15)) %>%
    layout(xaxis=list(anchor="y4",showspikes=TRUE, spikemode='across', spikesnap='cursor', spikethickness=0.5, spikedash='solid',showticklabels=FALSE),
           yaxis=list(side = "left", title = "Price",showspikes=TRUE, spikemode='across', 
                      spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis2 = list(side = "left",title = "MACD",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis3=list(side = "left", title = "MoneyFlow",showspikes=TRUE, spikemode='across', 
                       spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis4 = list(side = "left",title = "MFI",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           annotations=list(x=0.5, y=0.98, xref="paper", yref="paper",xanchor="left",text=Title,font=list(color="black"),showarrow=FALSE),
           hovermode = "x unified",plot_bgcolor="#D3D3D3", paper_bgcolor="#D3D3D3")%>%config(scrollZoom=TRUE)
  return(p)
}




FPVChart<- function(Pricedata,COTdata){
  for (i in 1:length(Pricedata[,1])) {       ##Color column for candlestick direction
    if (Pricedata$Close[i] >= Pricedata$Open[i]) {
      Pricedata$direction[i] = 'Increasing'
    }else{
      Pricedata$direction[i] = 'Decreasing'
    }
  }
  
  
  Pricedata_EMA60 <- FuncEMA60(Pricedata)
  Pricedata_EMA30 <- FuncEMA30(Pricedata)
  
  
  PriceChart<-plot_ly(data=Pricedata, x = ~Date, type='candlestick',open=~Open, close =~Close,high =~High, low =~Low,name="Candle Stick")%>%
    add_lines(x=Pricedata_EMA60$Date, y=Pricedata_EMA60$EMA60, name='EMA60', type='scatter', mode='lines',line=list(color='purple', width=2),inherit = F)%>%
    layout(xaxis=list(title="PriceDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE,rangeslider = list(visible = F)),
           yaxis=list(title="Price",side="left", showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'))
  
  VolumeChart<-plot_ly(data=Pricedata)%>%
    add_trace(x=~Date, y=~Volume, type='bar', color = ~direction, colors = c('#FF4136', '#3D9970'),yaxis='y')%>% 
    add_lines(x=~Date, y=~OpenInterest,name='DailyOpenInterest',type = 'scatter', mode = 'lines',line=list(color="pink", width=4),yaxis = 'y2')%>% 
    layout(xaxis = list(title="VolumeDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE),
           yaxis=list(title='Volume',side = 'left',showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis2=list(title='DailyOpenInterest',side='right',overlaying='y2',showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid'))
  
  P_VChart<-subplot(PriceChart,VolumeChart, nrows=2,shareX=TRUE, titleX=TRUE, titleY=TRUE, heights = c(0.4, 0.6))%>%
    layout(title = 'Price Chart',hovermode = "x unified"
    )%>%config(scrollZoom=TRUE)
  return(P_VChart)
}



FPVCOTChart<- function(Pricedata,COTdata){
  for (i in 1:length(Pricedata[,1])) {       ##Color column for candlestick direction
    if (Pricedata$Close[i] >= Pricedata$Open[i]) {
      Pricedata$direction[i] = 'Increasing'
    }else{
      Pricedata$direction[i] = 'Decreasing'
    }
  }
  
  
  Pricedata_EMA60 <- FuncEMA60(Pricedata)
  Pricedata_EMA30 <- FuncEMA30(Pricedata)
  Volume_EMA10<-na.omit(data.frame(Date=Pricedata$Date,EMA10=EMA(Pricedata$Volume, n=10)))
  
  PriceChart<-plot_ly(data=Pricedata, x=~Date, type='candlestick',open=~Open, close =~Close,high =~High, low =~Low, name="Candle Stick")%>%
    add_lines(x=Pricedata_EMA30$Date, y=Pricedata_EMA30$EMA30, name='EMA30', type='scatter', mode='lines',line=list(color='yellow', width=2),inherit = F)%>%
    add_lines(x=Pricedata_EMA60$Date, y=Pricedata_EMA60$EMA60, name='EMA60', type='scatter', mode='lines',line=list(color='purple', width=2),inherit = F)%>%
    layout(xaxis=list(title="PriceDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE,rangeslider = list(visible = F)), 
           yaxis=list(title="Price",side="left",showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'))
  
  VolumeChart<-plot_ly(data=Pricedata)%>%
    add_trace(x=~Date, y=~Volume, type='bar', color = ~direction, colors = c('#FF4136', '#3D9970'),name="VolumeBar",yaxis='y')%>% 
    add_lines(x=~Date, y=~OpenInterest,name='DailyOpenInterest',type='scatter', mode='lines',line=list(color='rgb(255,255,0)', width=4),yaxis = 'y2')%>%
    add_lines(x=Volume_EMA10$Date, y=Volume_EMA10$EMA10, name='Volume EMA10', type='scatter', mode='lines',line=list(color='#87CEFA', width=2),inherit = F)%>%
    layout(xaxis = list(title="VolumeDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE),
           yaxis=list(title = 'Volume',side = 'left',showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis2=list(title='DailyOpenInterest',side='right',overlaying='y2',showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'))
  
  OIChart <- plot_ly(COTdata) %>% 
    add_lines(x=~Date, y=~Open_Interest_All,name='TotalOpenInterest',type = 'scatter',mode = 'lines',line=list(color='rgb(255,20,147)', width=2), yaxis="y")%>%
    layout(xaxis=list(title="TotalOpenInterestDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE),
           yaxis=list(title='TotalOpenInterest',side="left", overlay="y5",showspikes=TRUE, spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'))
  
  COTChart<-plot_ly(data=COTdata, x=~Date, y=~COT_NonCom, name='Non-Commercial', type = 'scatter', mode = 'lines',line=list(color="red"))%>% 
    add_lines(data=COTdata, x=~Date, y=~COT_Com, name='Commercial', type = 'scatter', mode = 'lines',yaxis="y",line=list(color="blue"))%>%
    add_lines(data=COTdata, x=~Date, y=~COT_NonRept, name='Non-Reportable', type = 'scatter', mode = 'lines',yaxis="y",line=list(color="grey"))%>%
    layout(xaxis=list(title="COTDate",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5,spikedash='solid',showticklabels=TRUE),
           yaxis=list(title='COTNetPosition',side="left",overlay="y6",showspikes=TRUE,spikemode='across',spikesnap='cursor',spikethickness=0.5, spikedash='solid'))
  
  
  P_V_COTChart<-subplot(PriceChart,VolumeChart,OIChart,COTChart, nrows=4, shareX=TRUE,shareY=FALSE,titleX=FALSE,titleY=TRUE, heights=c(0.3,0.2,0.2,0.3), margin=0.0001)%>%
    layout(title = 'Price Chart',hovermode = "x unified",plot_bgcolor="#D3D3D3", paper_bgcolor="#D3D3D3")%>%config(scrollZoom=TRUE)
  
  return(P_V_COTChart)
  
}


MultiChart<-function(DataToBeTested){
  MultiChartList<-list()
  Titles<-names(DataToBeTested)
  
  for(i in 1:length(DataToBeTested)){
    MultiChartList<-c(MultiChartList, list(StockChart(DataToBeTested[[i]], Title = Titles[i])))
  }
  p<-subplot(MultiChartList,titleY=TRUE,shareX = FALSE)%>%layout(title="Multi-Period Chart", showlegend=FALSE)
  
  return(p)
}


ChartReplay<-function(Data, Title, PausePeriod=3,StartDate=NULL, UerInput="N"){
  #determine from which candlestick to start
  if(is.null(StartDate)==TRUE){StartDate<-map(Data, ~head(.x$Date, 88))%>%unlist()%>%max()}
  initialInd<-map(Data, ~tail(which(.x$Date<=StartDate), 1))
  maxInd<-map(Data, ~nrow(.x))%>%unlist()%>%min()
  mainData<-Data[[initialInd%>%which.max()]]
  
  Indices<-initialInd
  while(any(unlist(Indices)<=maxInd)){
    possibleError<-tryCatch( #This returns an object depending on error or not
      #The try part:
      expr = {
        #Check if there's an error, if not proceed, else goes to error function
        i<-Indices%>%unlist()%>%max()
        maindate<-mainData[i, "Date"]
        Indices <-map(Data, ~tail(which(.x$Date<=maindate), 1))
        
        if(length(Data)==1){
          newtotdata<-Data[[1]][max(c(1, i-399)):i,]
          res<-StockChart(newtotdata, Title)}
        else{
          newtotdata<-map2(Data, Indices, ~ slice(.x, max(c(1, .y-399)):.y))
          res<-MultiChart(newtotdata)}
        
        
        #evaluate based on UserInput or not
        if(UerInput=="Y"){
          proceed<-readline(prompt="Next bar? Y/N")
          if(proceed=="Y"){print(res);Indices<-map(Indices, ~.x+1);next}else{break}}
        else{
          cat("Chart is ready! ", paste0(names(Indices), ":", Indices), sep="")
          cat("\n");print(res);Indices<-map(Indices, ~.x+1);Sys.sleep(PausePeriod)}
      },
      
      #The error handling part
      error = function(e){
        cat("Chart is not ready for ", paste0(names(Indices), ":", Indices), ", please wait...", sep="")
        cat("\n")
        e #this is needed because it is the original error message, and it should be returned to possibleError variable
      }
    )
    if(inherits(possibleError,"error")){Indices<-map(Indices, ~.x+1)}
  }
}



getPerformance<-function(Pricedata){
  Pricedata$Date<-as.Date(Pricedata$Date)
  
  # Add a 'Week' column to group by week
  stock_data <- Pricedata %>%
    mutate(Week = floor_date(Date, unit = "week",week_start = getOption("lubridate.week.start", 5)))
  
  # Summarize weekly OHLC prices
  weekly_stock <- stock_data %>%
    group_by(Week) %>%
    summarize(
      High = max(High),
      Open = first(Open),
      Low = min(Low),
      Close = last(Close),
      Volume=sum(Volume)
    )
  
  # Print the summarized weekly stock data
  weekly_stock<-mutate(weekly_stock, Date=as.character(Week), .before='High')%>%select(-Week)
  
  # Calculate weekly returns based on closing prices
  weekly_stock <- weekly_stock %>%
    mutate(Return = (Close / lag(Close)) - 1)%>%
    filter(!is.na(Return))
  return(weekly_stock)
}

SectorPerformanceChart<-function(datalist){
  returns<-lapply(datalist,getPerformance)
  
  # Use map to add a Source column to each data frame and then bind them together
  allds<-returns%>%map(~select(.x, Date))%>%reduce(inner_join,by="Date")%>%unlist()%>%as.vector()
  
  long_df <-map(returns, ~filter(.x, Date %in% allds))%>%
    imap(~mutate(.x, Source = .y))%>%
    list_rbind()%>%arrange(Date)
  
  # Define a custom color palette
  custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#FFD700")
  
  fig <- long_df %>%
    plot_ly(
      x = ~Return, 
      y = ~Source, 
      color = ~Source,
      colors = custom_colors[1:length(datalist)],
      frame = ~Date, 
      type='bar'
    )%>%
    layout(xaxis=list(range=c(-0.1,0.1), tickvals=seq(-0.1,0.1, by=0.01)))%>%
    animation_opts(frame = 2000,redraw = FALSE)
  fig
}
