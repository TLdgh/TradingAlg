BlackScholes <- function(S, F=NULL, K, r, delt, TimeToExpiry, sig, type, SecurityType="STK"){
  if(SecurityType=="STK"){
    d1 <- (log(S/K) + (r -delt + sig^2/2)*TimeToExpiry) / (sig*sqrt(TimeToExpiry))
    d2 <- d1 - sig*sqrt(TimeToExpiry)
    
    if(type=="C"){
      value <- S*exp(-delt*TimeToExpiry)*pnorm(d1) - K*exp(-r*TimeToExpiry)*pnorm(d2)
      return(value)
    }else if(type=="P"){
      value <-  K*exp(-r*TimeToExpiry)*pnorm(-d2) - S*exp(-delt*TimeToExpiry)*pnorm(-d1)
      return(value)}
    
  }else if(SecurityType=="FUT"){
    d1 <- (log(F/K) + (sig^2/2)*TimeToExpiry) / (sig*sqrt(TimeToExpiry))
    d2 <- d1 - sig*sqrt(TimeToExpiry)
    
    if(type=="C"){
      value <- F*exp(-r*TimeToExpiry)*pnorm(d1) - K*exp(-r*TimeToExpiry)*pnorm(d2)
      return(value)
    }else if(type=="P"){
      value <-  K*exp(-r*TimeToExpiry)*pnorm(-d2) - F*exp(-r*TimeToExpiry)*pnorm(-d1)
      return(value)}
  }
}



Greeks<-function(S, K, r, delt, TimeToExpiry, sig, type){
  d1 <- (log(S/K) + (r -delt + sig^2/2)*TimeToExpiry) / (sig*sqrt(TimeToExpiry))
  d2 <- d1 - sig*sqrt(TimeToExpiry)
  value<-list()
  if (type=="C") {
    Delta<-exp(-delt*TimeToExpiry)*pnorm(d1)
    Gamma<- (exp(-delt*TimeToExpiry)*dnorm(d1))/(S*sig*sqrt(TimeToExpiry))
    Vega<- S*exp(-delt*TimeToExpiry)*dnorm(d1)*sqrt(TimeToExpiry)
    Theta<- delt*S*exp(-delt*TimeToExpiry)*pnorm(d1)-r*K*exp(-r*TimeToExpiry)*pnorm(d2)-(sig*K*exp(-r*TimeToExpiry)*dnorm(d2))/(2*sqrt(TimeToExpiry))
    Rho<-TimeToExpiry*K*exp(-r*TimeToExpiry)*pnorm(d2)
    Psi<- -TimeToExpiry*S*exp(-delt*TimeToExpiry)*pnorm(d1)
    
    value<-list(Delta,Gamma,Vega,Theta,Rho,Psi)
    names(value)<-c("Delta","Gamma","Vega","Theta","Rho","Psi")
    
  }else if (type=="S"){
    Delta<- -exp(-delt*TimeToExpiry)*pnorm(-d1)
    Gamma<- (exp(-delt*TimeToExpiry)*dnorm(d1))/(S*sig*sqrt(TimeToExpiry))
    Vega<- S*exp(-delt*TimeToExpiry)*dnorm(d1)*sqrt(TimeToExpiry)
    Theta<- delt*S*exp(-delt*TimeToExpiry)*pnorm(d1)-r*K*exp(-r*TimeToExpiry)*pnorm(d2)-(sig*K*exp(-r*TimeToExpiry)*dnorm(d2))/(2*sqrt(TimeToExpiry)) + r*K*exp(-r*TimeToExpiry) - delt*S*exp(-delt*TimeToExpiry)
    Rho<- -TimeToExpiry*K*exp(-r*TimeToExpiry)*pnorm(-d2)
    Psi<- TimeToExpiry*S*exp(-delt*TimeToExpiry)*pnorm(-d1)
    
    value<-list(Delta,Gamma,Vega,Theta,Rho,Psi)
    names(value)<-c("Delta","Gamma","Vega","Theta","Rho","Psi")
    
  }
  
  return(value)
  
}



DGTOptionModel<-function(S,K,r,delt,TimeToExpiry,sig,type,PredictedP,day){
  CurrentOptValue<-BlackScholes(S=S,K=K,r=r,delt=delt,TimeToExpiry=TimeToExpiry,sig=sig,type=type)
  GreekLetters<-Greeks(S=S,K=K,r=r,delt=delt,TimeToExpiry=TimeToExpiry,sig=sig,type=type)
  Delta <- GreekLetters[["Delta"]]
  Gamma <- GreekLetters[["Gamma"]]
  Theta <- GreekLetters[["Theta"]]/365
  
  epsilon<-PredictedP-S
  
  
  value<-CurrentOptValue+Delta*epsilon+1/2*Gamma*(epsilon)^2+day*Theta 
  
  return(value)
}



