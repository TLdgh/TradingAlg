#Initiate PL_Daily table
CreateNewT <- function(action, x, y){
  if (action < 0) {
    y[1] <- action 
    y[2] <- x
    y[3] <- y[1]*y[2]
    return(y)
  }else{
    y[1] <- action 
    y[2] <- x
    y[3] <- y[1]*y[2]
    
    return(y)}
}


#CurrentP <- 22.25   must be positive
#NewP <- 22.25     must be positive
#action <- -100  buy is 1 sell is -1
#PriceMultiplier <- 1  1 for stocks, 2 for MNQ, 5 for MES, 10 for MGC etc. 
AvgPL<- function(CurrentP,NewP,action,Leverage=1){
  PL_Daily <- data.frame(matrix(ncol=3, dimnames = list(NULL, c("NumOfT", "Price", "CashFlow"))))
  NewTransaction <- vector('numeric', length = 3)
  
  if(is.na(PL_Daily$NumOfT)[1]==TRUE){
    NewTransaction <-CreateNewT(action, NewP,NewTransaction)
    PL_Daily$NumOfT <-NewTransaction[1]
    PL_Daily$Price <-NewTransaction[2]
    PL_Daily$CashFlow <-NewTransaction[3]
    
    AverageP <- sum(PL_Daily$CashFlow)/ sum(PL_Daily$NumOfT)
    PnL <- Leverage*(CurrentP-AverageP)*sum(PL_Daily$NumOfT)
    
    print(PL_Daily)
    cat(if(sum(PL_Daily$NumOfT)>0){"Long"}else{"Short"}, sum(PL_Daily$NumOfT), "@ average price after last transation: ", AverageP, "\n")
    cat("Daily P&L ", PnL, "\n")
    
  }else{
    NewTransaction <-CreateNewT(action, NewP,NewTransaction)
    PL_Daily <-rbind(PL_Daily,NewTransaction)
    AverageP <- sum(PL_Daily$CashFlow)/ sum(PL_Daily$NumOfT)
    PnL <- Leverage*(CurrentP-AverageP)*sum(PL_Daily$NumOfT)
    
    print(PL_Daily)
    cat(if(sum(PL_Daily$NumOfT)>0){"Long"}else{"Short"}, sum(PL_Daily$NumOfT), "@ average price after last transation: ", AverageP, "\n")
    cat("Daily P&L ", PnL, "\n")
  }
}




MaxPosition<- function(Profit, LossPercent, Currentprice, Stoploss, Leverage=1){
  loss <- Profit*LossPercent
  
  result<-solve(abs(Currentprice-Stoploss)*Leverage, loss)
  cat("The maximum number of position you can open is: ", result, "\n")
}




PnL<-function(data){
  if(!"Leverage" %in% colnames(data)){data<-data%>%mutate(Leverage=1, .before = Quantity)}
  data<-data%>%select(Symbol,Leverage, Quantity, Price=T..Price,Comm=Comm.Fee, Code=Code)%>%na.omit()
  
  res<-data%>%group_by(Symbol)%>%mutate(Index = 1:n())%>%
    pivot_wider(names_from = Code, values_from =  Quantity, values_fill = 0)%>% 
    arrange(Symbol, Index)%>% 
    mutate(TotalStock = cumsum(O) + cumsum(C), 
           Sold = case_when(O == 0 ~ 0, # Nothing bought - cannot be sold
                            sum(abs(C)) > cumsum(abs(O)) ~ O, # Total closed is greater than total open till current - everything is sold
                            (cumsum(abs(O)) - abs(O)) > sum(abs(C)) ~ 0, # Total open excluding the current open is greater than total closed - nothing sold
                            TRUE ~ O - (cumsum(O) + sum(C))), 
           InStock = O - Sold,
           Preceeds=if_else(O!=0,-Price*Leverage*O,-Price*Leverage*C),
           PnL= if_else(O!=0, (Preceeds+Comm)*Sold/O, Preceeds+Comm)
    )
  print(res,n=Inf)
  
  res<-res%>%group_by(Symbol)%>%summarise(SubTotalPnL=sum(PnL))
  cat("Total Profit/Loss by Symbol:")
  print(res)
  cat("Total Profit/Loss:")
  print(res%>%summarise(TotalPnL=sum(SubTotalPnL)))
}
