# library(tidyverse)
library(MultNonParam)
library(rmutil)
dosimulation<-function(trialNum=10000,
   rgen=list(Cauchy=rcauchy,Gaussian=rnorm,Laplace=rlaplace),
   nvec=c(10,20,30,40),balance=1,shift=0,spread=1){
   count <- array(0, c(2,length(rgen),length(nvec)))
   dimnames(count)<-list(c("KS","AD"),names(rgen),as.character(nvec))
   for(jj in seq(length(nvec))){
      n <- nvec[jj]
      print(paste("cv",n))
#     determine the critical values for KS and AD tests
      x <- rgen[[1]](n)
      y <- (rgen[[1]](n*balance)+shift)*spread
      times_alt <- c(x, y)
      delta <- rep(1, (1+balance)*n)
      grp <- c(rep(0, n), rep(1, balance*n))
      cvs <- twosamplesurvpvs(times_alt, delta, grp, plotme = FALSE)$cv
#     print(paste("cvs",cvs))
      for (i in 1:trialNum) {
#        print(i)
         for(kk in seq(length(rgen))){
            x <- rgen[[kk]](n)
            y <- (rgen[[kk]](n*balance)+shift)*spread
            times_alt <- c(x, y)
            result <- twosamplesurvtests(times_alt, delta, grp)
            count[,kk,jj]<-count[,kk,jj]+(result>cvs)[1:2]
         }
      }
   }
   count<-count/trialNum
   return(count)
}
if(1==0){
set.seed(1)
count1<-dosimulation(balance=1,shift=1)
set.seed(1)
count2<-dosimulation(balance=2,shift=1)
set.seed(1)
count3<-dosimulation(balance=1,spread=2)
set.seed(1)
count4<-dosimulation(balance=2,spread=2)
}
myt<-function(n) return(rt(n,df=10))
myfweibull<-function(n) return((2*rbinom(n,1,.5)-1)*rweibull(n,shape=1.5))
distlist<-list(Cauchy=rcauchy, T10=myt, Laplace=rlaplace,
   FoldedWeibull=myfweibull, Gaussian=rnorm)

set.seed(1)
counta<-dosimulation(rgen=distlist,balance=1,shift=1)
set.seed(1)
countb<-dosimulation(rgen=distlist,balance=2,shift=1)
set.seed(1)
countc<-dosimulation(rgen=distlist,balance=1,spread=2)
set.seed(1)
countd<-dosimulation(rgen=distlist,balance=2,spread=2)
library(R.utils)
#Table 1
tab1<-cbind(wrap.array(counta,list(c(2,3),1)),wrap.array(countb,list(c(2,3),1)))
#Table 2
tab2<-cbind(wrap.array(countc,list(c(2,3),1)),wrap.array(countd,list(c(2,3),1)))
