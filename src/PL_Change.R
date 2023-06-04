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

