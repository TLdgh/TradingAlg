p <- 0.41

u <- 0.07
d <- 0.21
n<- 3
ntrial <- 100
L<- 10
x <- matrix(nrow = ntrial, ncol = n)
prob <- matrix(nrow = ntrial, ncol = n)



for(i in 1:ntrial){
  bernoulli <- rbinom(n, 1, p)  
  
  for(j in 1:n) {
  if(bernoulli[j] == 1){x[i,j] <- L*(1+u); prob[i,j] <- p}else{x[i,j] <- L*(1-d); prob[i,j] <- 1-p}
  }

}

head(x,20)
head(prob,20)
Unique_path <- unique(x*prob)
Unique_path

Ret <-vector("numeric", length = nrow(Unique_path))  
for(i in 1:nrow(Unique_path)) Ret[i] <- prod(Unique_path[i,])


sum(Ret)
cat("Expected value: ", L*((1+u)*p+(1-d)*(1-p))^n)




u <- 0.01
p <- 0.05
cat("Expected value: ", ((1+u)*p+(1-u)*(1-p))^1-1)
