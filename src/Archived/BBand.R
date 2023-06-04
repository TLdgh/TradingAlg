chartSeries(as.xts(subset(NQ1F, Date>="2022-02-09 06:00:00")[,-1], order.by = as.POSIXct(subset(NQ1F, Date>="2022-02-09 06:00:00")$Date)), TA=c(addBBands(n=60,maType="EMA",sd=2)))
