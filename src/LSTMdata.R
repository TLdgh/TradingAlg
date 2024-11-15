x=read.csv("Data/NQ/QQQ_data.csv")%>%arrange(Date)
x=x%>%mutate(MACD(Cl(x), nFast = 10, nSlow = 20, nSig = 7, maType = EMA, percent = FALSE)%>%
               as.data.frame()%>%transmute(MACD_Histogram=(macd-signal)*2))%>%na.omit()

df=x%>%mutate(group=cumsum(MACD_Histogram * lag(MACD_Histogram, default = first(MACD_Histogram))<=0))%>%
  group_by(group)%>%group_split()%>%keep(~nrow(.x)>5 && all(.x$MACD_Histogram<=0))


newdf=bind_rows(df)%>%select(MACD_Histogram,group)


unique_groups=unique(newdf$group)
res=lapply(1:(length(unique_groups)-1), function(i){
  y=newdf%>%filter(group %in% unique_groups[i:(i+1)])
  
  newdf2<-lapply(1:1000, function(j){
    df=y
    pairs=unique(df$group)
    df=df%>%mutate(MACD_Histogram=case_when(
      df$group==pairs[1] ~runif(length(df$MACD_Histogram),0.05,1.5)*df$MACD_Histogram,
      df$group==pairs[2] ~runif(length(df$MACD_Histogram),0.05,2)*df$MACD_Histogram)
    )
    df=df%>%mutate(group=paste0(i,"_",group,"_",j))
    return(df)
  })%>%bind_rows()
  return(newdf2)
})

res=res%>%bind_rows()
res=res%>%mutate(group=as.numeric(factor(group, levels = unique(res$group))))
write.csv(res, "Data/NQ/your_data.csv", row.names = FALSE)
