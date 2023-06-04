SignalPlot <- function(Pricedata,AddSignal=FALSE){
  source("C:\\R\\Script\\StockPlotFunction_Candy.R") #everytime we run a function from a different script, we must run this command
  Title<-names(Pricedata)
  Pricedata<-read.csv(file = RawDataLocation[[Title]], header = T)
  Pricedata<-Pricedata[,1:6]
  colnames(Pricedata) <- c("Date", "Open", "High","Low", "Close", "Volume")
  Pricedata<-Pricedata[order(Pricedata$Date, decreasing = FALSE),]
  
  #plot only the price  
  priceplot<-plot_ly(data=Pricedata, x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
    layout(title=Title,xaxis = list(rangeslider = list(visible = F),showticklabels=FALSE))
  
  #plot the price with the signals  
  if(AddSignal==TRUE){
    Pricedata_macd<-PricedataMACD(Pricedata)
    #Pricedata_vmacd<-VMACD(Pricedata)
    Pricedata_MFI<-PricedataMFI(Pricedata)
    
    Pricedata<- merge(Pricedata,Pricedata_macd, by="Date")
    #Pricedata<- merge(Pricedata,Pricedata_vmacd, by="Date")
    Pricedata<- merge(Pricedata,Pricedata_MFI, by="Date")
    Pricedata<-na.omit(Pricedata)
    
    if(Pricedata$MACD[1]>=0){Pricedata$MACDdirection[1]<-"green"}else{Pricedata$MACDdirection[1] = "red"}
    if(Pricedata$MFI[1]>=0){Pricedata$MFIdirection[1]<-"green"}else{Pricedata$MFIdirection[1] = "red"}
    for (i in 2:nrow(Pricedata)){       ##Color column for MACD, VMACD or MFI direction
      if (Pricedata$MACD[i] >= 0){
        if(Pricedata$MACD[i]>Pricedata$MACD[i-1]){Pricedata$MACDdirection[i]<-"green"}else{Pricedata$MACDdirection[i]<-"palegreen"}
      }else if(Pricedata$MACD[i]<0){
        if(Pricedata$MACD[i]<Pricedata$MACD[i-1]){Pricedata$MACDdirection[i]<-"red"}else{Pricedata$MACDdirection[i]<-"lightpink"}
      }
      #if (Pricedata$VMACD[i] >= 0) {
      #Pricedata$VMACDdirection[i] = "green"
      #}else{Pricedata$VMACDdirection[i] = "red"}
      
      if (Pricedata$MFI[i] >= 0){
        if(Pricedata$MFI[i]>Pricedata$MFI[i-1]){Pricedata$MFIdirection[i]<-"green"}else{Pricedata$MFIdirection[i]<-"palegreen"}
      }else if(Pricedata$MFI[i]<0){
        if(Pricedata$MFI[i]<Pricedata$MFI[i-1]){Pricedata$MFIdirection[i]<-"red"}else{Pricedata$MFIdirection[i]<-"lightpink"}
      }
    }
    priceplot<-plot_ly(data=Pricedata, x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
      layout(title=Title,xaxis = list(rangeslider = list(visible = F),showticklabels=FALSE))
    
    macdplot<-plot_ly(data=Pricedata, x=~Date)%>%
      add_trace(y=~DIFF,name="DIFF",type="scatter",mode = 'lines',line=list(color="orange", width=2))%>%
      add_trace(y=~DEA,name="DEA",type="scatter",mode = 'lines',line=list(color="00BDFF", width=2))%>%
      add_bars(y=~MACD,name="MACD", marker=list(color=~MACDdirection))%>%
      layout(xaxis = list(rangeslider = list(visible = F)))
    
    #vmacdplot<- plot_ly(data=Pricedata,x=~Date)%>%
    #add_trace(y=~VDIFF,name="VDIFF",type="scatter",mode = 'lines',line=list(color="orange", width=2))%>%
    #add_trace(y=~VDEA,name="VDEA",type="scatter",mode = 'lines',line=list(color="00BDFF", width=2))%>%
    #add_bars(y=~VMACD,name="VMACD",marker=list(color=~VMACDdirection))%>%
    #layout(xaxis = list(rangeslider = list(visible = F)))
    
    MFIlines<-function(y, color="black"){list(type="line",x0=0, x1=1, y0=y,y1=y, xref="paper", line=list(color=color))} #this gives the lines in MFI chart
    MFIChart<-plot_ly(data=Pricedata, x=~Date)%>%
      add_bars(y=~MFI,name="MFI", marker=list(color=~MFIdirection))%>%
      layout(xaxis = list(rangeslider = list(visible = F)), shapes=list(MFIlines(y=30), MFIlines(y=-30)))
    
    allplot<-subplot(priceplot, macdplot, MFIChart, nrows=3, shareX = TRUE, heights = c(0.4,0.4,0.2)) %>%
      layout(xaxis=list(anchor="y3",showspikes=TRUE, spikemode='across', spikesnap='cursor', spikethickness=0.5, spikedash='solid'),
             yaxis=list(side = "left", title = "Price",showspikes=TRUE, spikemode='across', 
                        spikesnap='cursor',spikethickness=0.5, spikedash='solid'), 
             yaxis2 = list(side = "left",title = "VMACD",showspikes=TRUE, spikemode='across', 
                           spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
             yaxis3 = list(side = "left",title = "MACD",showspikes=TRUE, spikemode='across', 
                           spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
             annotations=list(x=0.5, y=0.95, xref="paper", yref="paper",xanchor="left",text=Title,font=list(color="yellow"),showarrow=FALSE),
             hovermode = "x unified", plot_bgcolor="#262625", paper_bgcolor="#262625")%>%config(scrollZoom=TRUE)
  }
  else{allplot<-priceplot}
  
  return(allplot)
}


MultiSignalChart<-function(DataToBeTested){
  MultiChartList<-list()
  Titles<-names(DataToBeTested)
  
  for(i in 1:length(DataToBeTested)){
    MultiChartList<-c(MultiChartList, list(SignalPlot(DataToBeTested[i])))
  }
  p<-subplot(MultiChartList,titleY=TRUE,shareX = FALSE)%>%layout(title="Multi-Period Chart", showlegend=FALSE)
  
  return(p)
}




