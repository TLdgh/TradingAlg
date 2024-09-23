SignalPlot <- function(Pricedatafile, VIXfile=NULL){
  Pricedata<-read.csv(Pricedatafile, header = TRUE)%>%select(1:6)
  colnames(Pricedata) <- c("Date", "Open", "High","Low", "Close", "Volume")
  Pricedata<-Pricedata[order(Pricedata$Date, decreasing = FALSE),]%>%mutate(Return=log(Close/lag(Close)))
  
  
  #plot the price with the signals  
  Pricedata_macd<-PricedataMACD(Pricedata)
  Pricedata_MFI<-PricedataMFI(Pricedata)
  Pricedata<- left_join(Pricedata,Pricedata_macd, by="Date")
  Pricedata<- left_join(Pricedata,Pricedata_MFI, by="Date")
  
  vix=read.csv(VIXfile, header = TRUE)
  vix$DATE=as.character(as.Date(vix$DATE, format="%m/%d/%Y"))
  vix=select(vix, c("DATE","CLOSE"))
  colnames(vix)=c("Date","VIX")
  Pricedata<- left_join(Pricedata,vix, by="Date")
  Pricedata$VIX=na.approx(Pricedata$VIX)
  
  vix_macd<-as.data.frame(MACD(Pricedata$VIX, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE))
  vix_macd$MACD <- (vix_macd$macd - vix_macd$signal)*2
  vix_macd$Date <- Pricedata[order(Pricedata$Date, decreasing = F),]$Date
  colnames(vix_macd) <- c("vix_DIFF", "vix_DEA", "vix_MACD", "Date")
  
  Pricedata<-left_join(Pricedata, vix_macd, by="Date")
  Pricedata<-na.omit(Pricedata)
  Pricedata<-Pricedata%>%mutate(VIX_Low=(Close*(1+qnorm((1-0.686)/2)*VIX*sqrt(1/252)/100)), 
                                VIX_High=(Close*(1-qnorm((1-0.686)/2)*VIX*sqrt(1/252)/100))) #note I'm using z_alpha/2
  
  
  Pricedata<-Pricedata%>%mutate(across(c(MACD,MFI,vix_MACD), MakeDirection, .names="{col}_Direction"))
  
  priceplot<-plot_ly(data=Pricedata, x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
    add_lines(x=Pricedata$Date, y=Pricedata$VIX_Low, name='VIX_Low', type='scatter', mode='lines',
              line=list(color='black', width=1),inherit = F)%>%
    add_lines(x=Pricedata$Date, y=Pricedata$VIX_High, name='VIX_High', type='scatter', mode='lines',
              line=list(color='black', width=1),inherit = F)%>%
    layout(xaxis = list(rangeslider = list(visible = F),showticklabels=FALSE))
  
  macdplot<-plot_ly(data=Pricedata, x=~Date)%>%
    add_trace(y=~DIFF,name="DIFF",type="scatter",mode = 'lines',line=list(color="orange", width=2))%>%
    add_trace(y=~DEA,name="DEA",type="scatter",mode = 'lines',line=list(color="00BDFF", width=2))%>%
    add_bars(y=~MACD,name="MACD", marker=list(color=~MACD_Direction))%>%
    layout(xaxis = list(rangeslider = list(visible = F)))
  
  vix_macdplot<-plot_ly(data=Pricedata, x=~Date)%>%
    add_trace(y=~vix_DIFF,name="vix_DIFF",type="scatter",mode = 'lines',line=list(color="orange", width=2))%>%
    add_trace(y=~vix_DEA,name="vix_DEA",type="scatter",mode = 'lines',line=list(color="00BDFF", width=2))%>%
    add_bars(y=~vix_MACD,name="vix_MACD", marker=list(color=~vix_MACD_Direction))%>%
    layout(xaxis = list(rangeslider = list(visible = F)))
  
  MFIlines<-function(y, color="black"){list(type="line",x0=0, x1=1, y0=y,y1=y, xref="paper", line=list(color=color))} #this gives the lines in MFI chart
  MFIChart<-plot_ly(data=Pricedata, x=~Date)%>%
    add_bars(y=~MFI,name="MFI", marker=list(color=~MFI_Direction))%>%
    layout(xaxis = list(rangeslider = list(visible = F)), shapes=list(MFIlines(y=30), MFIlines(y=-30)))
  
  allplot<-subplot(priceplot, macdplot, vix_macdplot, MFIChart, nrows=4, shareX = TRUE, heights = c(0.4,0.2,0.2,0.2)) %>%
    layout(xaxis=list(anchor="y4",showspikes=TRUE, spikemode='across', spikesnap='cursor', spikethickness=0.5, spikedash='solid'),
           yaxis=list(side = "left", title = "Price",showspikes=TRUE, spikemode='across', 
                      spikesnap='cursor',spikethickness=0.5, spikedash='solid'), 
           yaxis2 = list(side = "left",title = "MACD",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis3 = list(side = "left",title = "vix_MACD",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           yaxis4 = list(side = "left",title = "MFI",showspikes=TRUE, spikemode='across', 
                         spikesnap='cursor',spikethickness=0.5, spikedash='solid'),
           hovermode = "x unified", plot_bgcolor="#262625", paper_bgcolor="#262625")%>%config(scrollZoom=TRUE)
  
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




