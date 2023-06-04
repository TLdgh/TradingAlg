increment <- 0.1
Principle <- 1760
leverage <- 12.5

num <- seq(1,251, increment)
vector <-matrix(0, nrow = length(num), ncol = 5)
stock_p <- 250
NShare <-Principle*leverage/stock_p

checkEquity <- function(x, i){
  if(x[i,5]>=x[i,4]){
    x[i,5]<-x[i,5]
  }else{
    x[i,5]<-x[i,5]+x[i,1]}
  return(x)
}

checkShares <- function(x, y, i){
  if(y[i,5]>=y[i,4]){
    x<-x
  }else{
    x<-x-1}
  return(x)
}


for (i in 1:length(num)) {
  vector[i,1] <- stock_p
  vector[i,2] <- NShare
  vector[i,3] <- stock_p*NShare
  vector[i,4] <- vector[i, 3]*0.35
  vector[i,5] <- Principle-NShare*(250-stock_p)
  stock_p <- stock_p-increment
  NShare <- checkShares(NShare, vector, i)
  vector <- checkEquity(vector, i)

}
colnames(vector) <- c("Stock Price", "Shares", "Total Market Value", "Maintenance Margin", "Equity")
Account <-as.data.frame(vector) 

print(vector)
