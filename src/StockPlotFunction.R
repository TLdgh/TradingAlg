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
                line=list(color='grey', width=2),inherit = F)%>%
      add_lines(x=Pricedata_BOLL$Date, y=Pricedata_BOLL$upB, name='BOLL_upB', type='scatter', mode='lines',
                line=list(color='grey', width=2),inherit = F)
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
                line=list(color='silver', width=2),inherit = F)
  }
}


PricedataMACD <- function (Pricedata){
  Pricedata_macd <- as.data.frame(MACD(Cl(Pricedata[order(Pricedata$Date, decreasing = F),]), nFast = 5, nSlow = 20, nSig = 10, maType = EMA, percent = FALSE))
  Pricedata_macd$MACD <- (Pricedata_macd$macd - Pricedata_macd$signal)*2
  Pricedata_macd$Date <- Pricedata[order(Pricedata$Date, decreasing = F),]$Date
  colnames(Pricedata_macd) <- c("DIFF", "DEA", "MACD", "Date")
  return(Pricedata_macd)
}

VMACD <- function (Pricedata){
  Volume_macd <- as.data.frame(MACD(Vo(Pricedata[order(Pricedata$Date, decreasing = F),]), nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE))
  Volume_macd$MACD <- (Volume_macd$macd - Volume_macd$signal)
  Volume_macd$Date <- Pricedata[order(Pricedata$Date, decreasing = F),]$Date
  colnames(Volume_macd) <- c("VDIFF", "VDEA", "VMACD", "Date")
  return(Volume_macd)
}

PricedataMFI<-function(Pricedata){
  MFI<-MFI(HLC(Pricedata), volume = Pricedata$Volume, n=10)-50
  MFI<-na.omit(data.frame(Date=Pricedata$Date,MFI=MFI))
  return(MFI)
}

PricedataBOLL<-function(Pricedata){
  BOLL<- BBands(Cl(Pricedata), n=60, sd=2, maType = EMA)
  BOLL<-na.omit(data.frame(Date=Pricedata$Date,PctB=BOLL[,"pctB"], lwB=BOLL[,"dn"], upB=BOLL[,"up"]))
  return(BOLL)
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
  Alldata<- merge(Pricedata,Pricedata_macd, by="Date")
  Alldata<- merge(Alldata,Pricedata_MFI, by="Date")
  Alldata<-na.omit(Alldata)
  
  if(Alldata$MACD[1]>=0){Alldata$MACDdirection[1]<-"green"}else{Alldata$MACDdirection[1] = "red"}
  if(Alldata$MFI[1]>=0){Alldata$MFIdirection[1]<-"green"}else{Alldata$MFIdirection[1] = "red"}
  if(Alldata$Close[1] >= Alldata$Open[1]){Alldata$VOLdirection[1] = 'green'}else{Alldata$VOLdirection[1] = 'red'}
  
  for (i in 2:nrow(Alldata)) {       ##Color column for MACD, VMACD, MFI and Volume direction
    if (Alldata$MACD[i] >= 0) {
      if(Alldata$MACD[i]>Alldata$MACD[i-1]){Alldata$MACDdirection[i]<-"green"}else{Alldata$MACDdirection[i]<-"palegreen"}
    }else if(Alldata$MACD[i]<0){
      if(Alldata$MACD[i]<Alldata$MACD[i-1]){Alldata$MACDdirection[i]<-"red"}else{Alldata$MACDdirection[i]<-"lightpink"}
    }
    
    #if (Alldata$VMACD[i] >= 0) {
    #Alldata$VMACDdirection[i] = "green"
    #}else{Alldata$VMACDdirection[i] = "red"}
    
    if (Alldata$MFI[i] >= 0){
      if(Alldata$MFI[i]>Alldata$MFI[i-1]){Alldata$MFIdirection[i]<-"green"}else{Alldata$MFIdirection[i]<-"palegreen"}
    }else if(Alldata$MFI[i]<0){
      if(Alldata$MFI[i]<Alldata$MFI[i-1]){Alldata$MFIdirection[i]<-"red"}else{Alldata$MFIdirection[i]<-"lightpink"}
    }
    
    if (Alldata$Volume[i]<2*Alldata$Volume[i-1]){
      if(Alldata$Close[i] >= Alldata$Open[i]){Alldata$VOLdirection[i] = 'green'}
      else{Alldata$VOLdirection[i] = 'red'}
    }
    else{Alldata$VOLdirection[i] = 'gold'}
    
  }
  
  
  VolumeChart<-plot_ly(data=Alldata, x=~Date, y=~Volume, type='bar', marker=list(color = ~VOLdirection))%>%
    layout(xaxis=list(showticklabels=FALSE))
  
  MACDChart<-plot_ly(data=Alldata, x=~Date)%>%
    add_trace(y=~DIFF,name="DIFF",type="scatter",mode = 'lines', line=list(color="orange", width=2))%>%
    add_trace(y=~DEA,name="DEA",type="scatter",mode = 'lines', line=list(color="blue", width=2))%>%
    add_bars(y=~MACD,name="MACD", marker=list(color=~MACDdirection))%>%
    layout(xaxis = list(rangeslider = list(visible = F)))
  
  #VMACDChart<-plot_ly(data=Alldata,x=~Date)%>%
  #add_trace(y=~VDIFF,name="VDIFF",type="scatter",mode = 'lines', line=list(color="orange", width=2))%>%
  #add_trace(y=~VDEA,name="VDEA",type="scatter",mode = 'lines', line=list(color="blue", width=2))%>%
  #add_bars(y=~VMACD,name="VMACD",marker=list(color=~VMACDdirection))%>%
  #layout(xaxis = list(rangeslider = list(visible = F)))
  
  MFIlines<-function(y, color="black"){list(type="line",x0=0, x1=1, y0=y,y1=y, xref="paper", line=list(color=color))} #this gives the lines in MFI chart
  MFIChart<-plot_ly(data=Alldata, x=~Date)%>%
    add_bars(y=~MFI,name="MFI", marker=list(color=~MFIdirection))%>%
    layout(xaxis = list(rangeslider = list(visible = F)), shapes=list(MFIlines(y=34), MFIlines(y=-34)))
  
  p<-subplot(PriceChart(Alldata, Title), VolumeChart, MACDChart, MFIChart, nrows=4, shareX=TRUE, heights = c(0.4, 0.15, 0.15, 0.15)) %>%
    layout(xaxis=list(anchor="y4",showspikes=TRUE, spikemode='across', spikesnap='cursor', spikethickness=0.5, spikedash='solid',showticklabels=FALSE),
           yaxis=list(side = "left", title = "Price",showspikes=TRUE, spikemode='across', 
                      spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis2=list(side = "left", title = "Volume",showspikes=TRUE, spikemode='across', 
                       spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis3 = list(side = "left",title = "MACD",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis4 = list(side = "left",title = "MFI",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           #yaxis5 = list(side = "left",title = "VMACD",showspikes=TRUE, spikemode='across', 
           #spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
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


ChartReplay<-function(Pricedata,AuxillaryData=NULL, Title, PausePeriod=3, StartCandle=NULL,StartDate=NULL, UerInput="N"){
  #determine from which candlestick to start
  if(is.null(StartCandle)==TRUE & is.null(StartDate)==TRUE){i<-1}
  else if(is.null(StartCandle)==FALSE){i<-StartCandle}
  else{i<-which(Pricedata$Date==StartDate)}
  
  while(i<=nrow(Pricedata)){
    possibleError<-tryCatch( #This returns an object depending on error or not
      #The try part:
      expr = {
        #Check if there's an error, if not proceed, else goes to error function
        maindate<-Pricedata[i,"Date"]
        if(i<=399){
          if(is.null(AuxillaryData)==TRUE){res<-StockChart(Pricedata[1:i,], Title)}else{res<-MultiChart(list(MainChart=Pricedata[1:i,], AuxChart=subset(AuxillaryData, Date<=maindate)))}
        }else{
          if(is.null(AuxillaryData)==TRUE){res<-StockChart(Pricedata[(i-399):i,], Title)}else{res<-MultiChart(list(MainChart=Pricedata[(i-399):i,], AuxChart=subset(AuxillaryData, Date<=maindate)))}
        }
        
        #evaluate based on UserInput or not
        if(UerInput=="Y"){
          proceed<-readline(prompt="Next bar? Y/N")
          if(proceed=="Y"){print(res);i<-i+1;next}else{break}
        }else{print(paste0("Chart is ready! Candlestick: ",i));print(res);i<-i+1;Sys.sleep(PausePeriod)}
      },
      
      #The error handling part
      error = function(e){
        print(paste0("Chart is not ready for the ",i, "th candlestick. Please wait..."))
        e #this is needed because it is the original error message, and it should be returned to possibleError variable
      }
    )
    if(inherits(possibleError,"error")){i<-i+1}
  }
}
