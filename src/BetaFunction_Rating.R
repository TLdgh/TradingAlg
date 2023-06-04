beta.ineq <- function(a, b, c, d, delta){
  integrand <- function(x) { dbeta(x, a, b)*pbeta(x-delta, c, d) }
  integrate(integrand, delta, 1, rel.tol=1e-4)$value
}

Return_1<-c(subset(QQQ_daily, Date>="2004-08-13" & Date<="2004-10-07")$Close
            -subset(QQQ_daily, Date>="2004-08-13" & Date<="2004-10-07")$Open)
Return_2<-c(subset(QQQ_daily, Date>="2004-10-14" & Date<="2004-12-14")$Close
            -subset(QQQ_daily, Date>="2004-10-14" & Date<="2004-12-14")$Open)

a<-sum(Return_1>0)+1
b<-sum(Return_1<=0)+1

c<-sum(Return_2>0)+1
d<-sum(Return_2<=0)+1
