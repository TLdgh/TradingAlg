SectorHist<-function(df,title,numplot){
  minchange=min(df$Change)
  maxchange=max(df$Change)
  bin_size<-(maxchange-minchange)/50 #divide into 50 equal-length intervals.
  
  negative_change <- subset(df, Change <= 0)
  positive_change <- subset(df, Change > 0)
  
  Nbin_minchange=ceiling(abs(minchange/bin_size))
  Nbin_maxchange=ceiling(abs(maxchange/bin_size))
  
  # Create the distribution plot based on the percentage change of the price, this gives the market breadth.
  p<-plot_ly() %>%
    add_histogram(data = negative_change, x = ~Change, marker = list(color = 'red', line=list(color="black", width=1)), name = 'Negative', xbins=list(start = (0-Nbin_minchange*bin_size), end = 0, size = bin_size)) %>%
    add_histogram(data = positive_change, x = ~Change, marker = list(color = 'green', line=list(color="black", width=1)), name = 'Positive',xbins=list(start = 0, end = (0+Nbin_maxchange*bin_size), size = bin_size)) %>%
    layout(barmode = 'overlay',
           xaxis=list(tickformat=".2%"),
           annotations = list(list(
             x = (minchange+maxchange)/2,
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


#连续两天上涨的股票分析，如果数量很多且平均涨幅很大，表明板块赚钱效应很好
WinningEffect<-function(data1,data2){
  D1D2=lapply(unique(data2$Sector), function(s){
    map(list(data1,data2), function(d){
      filter(d,Sector==s)%>%mutate(WeightCap=Market.Cap/sum(Market.Cap))%>%select(Ticker,Sector,WeightCap,Change)}
    )%>%
      reduce(., right_join, by="Ticker")
  })
  
  Winner_D1D2<-D1D2%>%map(~filter(.x, Change.x>0 & Change.y>0)) #Change.x is the D1 change, Change.y is D2
  Winner_D2<-D1D2%>%map(~filter(.x, Change.y>0)) #Change.y is D2
  
  
  finalres=pmap(list(Winner_D2, Winner_D1D2, D1D2), .f=function(d1, d2, d3){
    res=data.frame(
      Sector=unique(d2$Sector.y),
      PctOneGain=nrow(d1)/nrow(d3), #今日上涨数比例
      PctTwoGains=nrow(d2)/nrow(d3), #2日连续上涨数比例
      D2Perf=d2%>%summarise(res=sum(Change.y*WeightCap.y, na.rm = TRUE))%>%as.numeric()#昨日上涨今日表现市值加权平均涨幅
    )
    return(res)}
  )%>%bind_rows()
  
  finallist=list(By_PctTwoGains=finalres%>%arrange(desc(PctTwoGains)), By_D2Perf=finalres%>%arrange(desc(D2Perf)))
  
  return(finallist)
}







data_old=read.csv("Data/OriginalStockData/US/SectorDistribution/distribution_20241106.csv")
data_new=read.csv("Data/OriginalStockData/US/SectorDistribution/distribution_20241107.csv")


#衡量赚钱效应
WinningEffect(data_old,data_new)


#板块涨跌分布
plotlist<-list(all=SectorHist(df=data_new, title="All", numplot=1))
for (s in seq_along(unique(data_new$Sector))){
  nam<-unique(data_new$Sector)[s]
  plotlist[[nam]] <- SectorHist(df=subset(data_new, Sector==nam), title=nam, numplot=s+1)
}
subplot(plotlist, nrows = 4,shareX = FALSE, shareY = FALSE)




#generate distribution manually
subfile=read.csv("Data/OriginalStockData/US/SectorDistribution/template.csv")
for(d in CommDates[-1]){
  res=lapply(All, function(stock){
    file_path=paste0(getwd(),"/Data/OriginalStockData/US/", stock,"_daily.csv")
    file=read.csv(file_path,header = TRUE)
    ind=which(file$Index==d)
    if(is_empty(ind)==TRUE){print(stock);stop("correct data.")}
    Price=file[ind, 5]
    PrePrice=file[ind-1, 5]
    Change=(Price-PrePrice)/PrePrice
    return(data.frame(Ticker=stock, Price, Change))
  })%>%bind_rows()
  
  res[which(res$Ticker %in% c("BRK B", "BF B")), "Ticker"]=c("BRK-B", "BF-B")
  res=left_join(subfile, res, by="Ticker")
  
  write.csv(res, file = paste0("Data/OriginalStockData/US/SectorDistribution/distributionSUB_",gsub("-","",d),".csv"))
}
