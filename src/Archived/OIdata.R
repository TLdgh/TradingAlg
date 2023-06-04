OIData<-ts(c(COT$Open_Interest_All), frequency=17)    #tried a few periods, 62,17, and 22 looks the best

OI_decomp_ma<-decompose(OIData, type="additive")

plot(OI_decomp_ma)

OI_trend<-OI_decomp_ma$trend    #get the trend using Moving Average
OI_seasonal<-OI_decomp_ma$seasonal #get the seasonal part
OI_random<-na.omit(OI_decomp_ma$random) #the random part

acf(OI_random)
pacf(OI_random)

OI_NOtrend<-OIData-OI_trend    #remove the trend
OI_NOtrend<-matrix(OI_NOtrend, ncol = 62, byrow = TRUE)  
OI_NOtrend<-apply(OI_NOtrend, MARGIN=2,FUN=na.omit)

c_t<-sum(as.numeric(na.omit(OI_NOtrend)))/744   #calculate the constant
s_t<-colMeans(OI_NOtrend)-c_t   #further remove the constant to estimate the seasonal part


Lambda_p <- matrix(0,nrow = 7, ncol = 7)
  
for (i in 1:7) {
  for (j in 1:7) {
      Lambda_p[i,j]<-OI_sample_acf[abs(i-j)+1]
  }
}

b_p<-matrix(OI_sample_acf[2:8],ncol = 1)
solve(Lambda_p)%*%b_p

OI_sample_mean<- mean(COT$Open_Interest_All)










