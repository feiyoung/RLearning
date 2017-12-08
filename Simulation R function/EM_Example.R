setwd('F:\\我的桌面文件\\我的桌面2\\代码文件\\R语言\\')

getwd()
# 源于http://stats.stackexchange.com/questions/55132/em-algorithm-manually-implemented
# EM algorithm manually

## simulate data
d <- 2
n <- 4000
p <- 2/3
q <- 1 - p
Z <- rbinom(n, 1, p)
Mu1 <- 0
Mu2 <- 1
Sigma1 <- sqrt(1)
Sigma2 <- sqrt(4)
x <- numeric(length=n)
set.seed(1234)
for(i in 1:n){
  if(Z[i] == 1) x[i] <- rnorm(1, Mu1, Sigma1)
  if(Z[i] == 0) x[i] <- rnorm(1, Mu2, Sigma2)
}
# initial values
pi1<-0.1
pi2<-0.9
mu1<--0.1
mu2<-0.01
sigma1<-sqrt(0.01)
sigma2<-sqrt(0.02)
loglik<- rep(NA, 1000)
loglik[1]<-0
mysum <- function(x) {
  sum(x[is.finite(x)])
}
loglik[2]<-mysum(pi1*(log(pi1)+log(dnorm(dat,mu1,sigma1))))+mysum(pi2*(log(pi2)+log(dnorm(dat,mu2,sigma2))))

logdnorm <- function(x, mu, sigma) {
  mysum(sapply(x, function(x) {log(dnorm(x, mu, sigma))}))  
}
tau1<-0
tau2<-0
#k<-1
k<-2

# loop
while(abs(loglik[k]-loglik[k-1]) >= 1e-6) {
  # E step
  tau1<-pi1*dnorm(x,mean=mu1,sd=sigma1)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(x,mean=mu2,sd=sigma2))
  tau2<-pi2*dnorm(x,mean=mu2,sd=sigma2)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(x,mean=mu2,sd=sigma2))
  tau1[is.na(tau1)] <- 0.5
  tau2[is.na(tau2)] <- 0.5
  
  # M step
  pi1<-mysum(tau1)/length(x)
  pi2<-mysum(tau2)/length(x)
  
  mu1<-mysum(tau1*x)/mysum(tau1)
  mu2<-mysum(tau2*x)/mysum(tau2)
  
  sigma1<- sqrt(mysum(tau1*(x-mu1)^2)/mysum(tau1))
  sigma2<- sqrt(mysum(tau2*(x-mu2)^2)/mysum(tau2))
  
  #  loglik[k]<-sum(tau1*(log(pi1)+log(dnorm(x,mu1,sigma1))))+sum(tau2*(log(pi2)+log(dnorm(x,mu2,sigma2))))
  loglik[k+1]<-mysum(tau1*(log(pi1)+logdnorm(x,mu1,sigma1)))+mysum(tau2*(log(pi2)+logdnorm(x,mu2,sigma2)))
  k<-k+1
}
c(pi1, pi2, mu1, mu2, sigma1, sigma2)

# compare
#install.packages('mixtools')
library(mixtools)
set.seed(1234)
gm<-normalmixEM(x,lambda=c(0.5,0.7),mu=c(-0.01,0.04),sigma=c(0.01,0.02),
                epsilon = 1e-6)
#gm<-normalmixEM(x,k=2,epsilon = 1e-06,arbvar = FALSE, fast=T)
gm$lambda
gm$mu
gm$sigma
gm$loglik

c(p,q,Mu1,Mu2,Sigma1,Sigma2) ##实际参数值
