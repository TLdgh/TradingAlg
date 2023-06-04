MaxPortfolio<-function(DataName, Bookcost){
  stock_daily<-data.frame()
  InitialP <- data.frame()
  
  for (i in 1:length(DataName)) {
    stock<-read.csv(file=paste("C:\\R\\Data\\US\\", DataName[i], ".csv", sep = ""), header = T)
    stock$Date<-as.POSIXct(stock$Date)
    stock<-stock[order(stock$Date, decreasing=FALSE),]
    stock<-cbind(symbol=DataName[i],stock)
    InitialP[1,i]<-tail(stock,1)$Close
    stock_daily<-rbind(stock_daily, stock)
  }
  colnames(InitialP)<-DataName
  
  Return<-stock_daily%>%group_by(symbol)%>%tq_transmute(select=Close, mutate_fun=periodReturn,period="daily",type="log",col_rename="ret")%>% spread(symbol, value = ret)
  Return<-na.omit(Return)
  Return<-as.xts(Return[-1,-1], order.by = Return$Date[-1])
  
  mean_ret <- colMeans(Return) #daily return
  CovM <- cov(Return)*252 #annual covariance matrix
  
  num_port <- 5000
  W <- matrix(nrow = num_port, ncol = length(DataName))
  port_returns <- vector('numeric', length = num_port)
  port_risk <- vector('numeric', length = num_port)
  sharpe_ratio <- vector('numeric', length = num_port)
  
  for (i in seq_along(port_returns)){
    wts <- runif(length(DataName))
    wts <- matrix(wts/sum(wts), nrow = 1)
    # Storing weight in the matrix
    W[i,] <- wts
    
    # Portfolio returns
    port_ret <- sum(wts * mean_ret)
    port_ret <- ((port_ret + 1)^252) - 1
    
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    
    # Creating and storing portfolio risk
    port_sd <- sqrt(wts%*%CovM%*% t(wts))
    port_risk[i] <- port_sd
    
    # Creating and storing Portfolio Sharpe Ratios
    # Assuming 0% Risk free rate
    sr <- port_ret/port_sd
    sharpe_ratio[i] <- sr
  }
  
  colnames(W) <- colnames(Return)
  
  # Storing the values in the table
  portfolio_values <-  data.frame(W, Return = port_returns,Risk = port_risk,SharpeRatio = sharpe_ratio)
  min_var <- portfolio_values[which.min(portfolio_values$Risk),]
  max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
  
  print(list("Minimum Variance Portfolio" = min_var, "Maximum Sharpe Ratio Portfolio" = max_sr))
  
  MaxSRCostTable<-rbind(InitialP, round(max_sr[1:length(DataName)],2) ,round(max_sr[1:length(DataName)]*Bookcost,2))
  
  MaxSRCostTable<-rbind(MaxSRCostTable,round(MaxSRCostTable[3,]/MaxSRCostTable[1,]))
  rownames(MaxSRCostTable) <- c("InitialP","Weight", "IndividualCost", "Quantity")
  MaxSRCostTable<-MaxSRCostTable[,order(MaxSRCostTable["Weight",])]
  print("Maximum Sharpe Ratio Portfolio Cost Table:")
  print(MaxSRCostTable)
  
  p_min_var <- min_var %>%
    gather(DataName, key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
    scale_y_continuous(labels = scales::percent) 
  
  p_max_sr <- max_sr %>%
    gather(DataName, key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Assets', y = 'Weights', title = "Maximum Sharpe Ratio Weights") +
    scale_y_continuous(labels = scales::percent) 
  
  p <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio))+geom_point()+theme_classic()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(labels = scales::percent)+
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization & Efficient Frontier")+
    geom_point(aes(x = Risk,y = Return), data = min_var, color = 'red')+
    geom_point(aes(x = Risk,y = Return), data = max_sr, color = 'red')+
    geom_text_repel(aes(x = Risk,y = Return), data = min_var, label="Minimum Variance Portfolio",color="red",size=5)+
    geom_text_repel(aes(x = Risk,y = Return), data = max_sr, label = "Maximum Sharpe Ratio Portfolio",color="red",size=5)
  
  ggarrange(p_min_var,p_max_sr,p, nrow=3,ncol=1)
  
}

