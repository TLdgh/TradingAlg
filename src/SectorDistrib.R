SectorHist<-function(df,title,numplot){
  minchange=min(df$Change)
  maxchange=max(df$Change)
  bin_size<-(maxchange-minchange)/50 #divide into 50 equal-length intervals.
  
  negative_change <- subset(df, Change <= 0)
  positive_change <- subset(df, Change > 0)
  
  Nbin_minchange=ceiling(abs(minchange/bin_size))
  Nbin_maxchange=ceiling(abs(maxchange/bin_size))
  
  # Create the distribution plot based on the percentage change of the price
  p<-plot_ly() %>%
    add_histogram(data = negative_change, x = ~Change, marker = list(color = 'red', line=list(color="black", width=1)), name = 'Negative', xbins=list(start = (0-Nbin_minchange*bin_size), end = 0, size = bin_size)) %>%
    add_histogram(data = positive_change, x = ~Change, marker = list(color = 'green', line=list(color="black", width=1)), name = 'Positive',xbins=list(start = 0, end = (0+Nbin_maxchange*bin_size), size = bin_size)) %>%
    layout(barmode = 'overlay',
           xaxis=list(tickformat=".2%"),
           annotations = list(list(
             x = 0,
             y=1,
             xref = paste0('x',numplot),
             yref = "paper",
             text = title,
             showarrow = FALSE,
             font = list(size = 12)
           ),
           list(
             x = minchange,
             y=0.8,
             xref = paste0('x',numplot),
             yref = "paper",
             text = paste0(round(nrow(negative_change)/nrow(df)*100,2),"%"),
             showarrow = FALSE,
             font = list(size = 24,color="red")
           ),
           list(
             x = maxchange,
             y=0.8,
             xref = paste0('x',numplot),
             yref = "paper",
             text = paste0(round(nrow(positive_change)/nrow(df)*100,2),"%"),
             showarrow = FALSE,
             font = list(size = 24,color="green")
           )),
           showlegend=FALSE,hovermode = "x unified"
    )%>%config(scrollZoom=TRUE)  # Overlay the two histograms
  return(p)
}



WinningEffect<-function(data1,data2){
  D1D2=lapply(unique(data1$Sector), function(s){
    map(list(data1,data2), function(d){
      filter(d,Sector==s)%>%mutate(WeightCap=Market.Cap/sum(Market.Cap))%>%select(Ticker,Sector,WeightCap,Change)}
    )%>%
      reduce(., right_join, by="Ticker")
  })
  
  Winner_D1D2<-D1D2%>%map(~filter(.x, Change.x>0 & Change.y>0)) #Change.x is the D1 change, Change.y is D2
  
  
  finalres=map2(Winner_D1D2, D1D2, .f=function(d1, d2){
    res=data.frame(
      Sector=unique(d1$Sector.x),
      PctTwoGains=nrow(d1)/nrow(d2), 
      D2Perf=d1%>%summarise(res=sum(Change.y*WeightCap.y, na.rm = TRUE))%>%as.numeric()#昨日涨停今日表现市值加权平均涨幅
    )
    return(res)}
  )%>%bind_rows()%>%arrange(desc(D2Perf))
  
  return(finalres)
}







data_old=read.csv("Data/OriginalStockData/US/SectorDistribution/distribution_20241017.csv")
data_new=read.csv("Data/OriginalStockData/US/SectorDistribution/distribution_20241018.csv")


#衡量赚钱效应
WinningEffect(data_old,data_new)


#板块涨跌分布
plotlist<-list(all=SectorHist(df=data_new, title="All", numplot=1))
for (s in seq_along(unique(data_new$Sector))){
  nam<-unique(data_new$Sector)[s]
  plotlist[[nam]] <- SectorHist(df=subset(data_new, Sector==nam), title=nam, numplot=s+1)
}
subplot(plotlist, nrows = 4,shareX = FALSE, shareY = FALSE)


