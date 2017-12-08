
# change point in generalized linear model --------------------------------
setwd('F:\\我的桌面文件\\我的桌面2\\代码文件\\R语言\\')
rm(list=ls())

# 广义线性模型 ------------------------------------------------------------------
## binomial case
set.seed(1234)
# n = 50, 500, 5000
n <- 50
p <- 10
beta <- 1:p
X <- matrix(rnorm(n*p), n, p)
eta <- X %*% beta
u <- 1- exp(-exp(eta))#  pnorm(eta) # 1/(1 + exp(-eta)) #
y <- sapply(u, function(x) rbinom(1, 1, x))
# logistic, probit, cloglog as link funciton
bino_gl <- glm(y~X+0, binomial(link = "cloglog"))  # fit effect is very good
summary(bino_gl)
sqrt(mean((coef(bino_gl)-beta)^2))

## poisson case
set.seed(1234)
# n = 50, 500, 5000
n <- 50
p <- 10
beta <- 1:p
X <- matrix(rnorm(n*p), n, p)
eta <- X %*% beta
u <- exp(eta)#pnorm(eta) #1/(1 + exp(-eta))
y <- sapply(u, function(x) rpois(1, x))
# log as link funciton
pois_gl <- glm(y~X+0, poisson(link = "log"))  # fit effect is very good
summary(pois_gl)
sqrt(mean((coef(pois_gl)-beta)^2))

## Inverse Gaussian case
#install.packages('statmod')
library(statmod)
set.seed(1234)
# n = 50, 500, 5000
n <- 50
p <- 10
beta <- 1:p
X <- matrix(rpois(n*p, 10), n, p)
eta <- X %*% beta
u <- sqrt(1/eta)#(-1)/(eta) #pnorm(eta) #1/(1 + exp(-eta))
y <- sapply(u, function(x) rinvgauss(1, x, 1))
# log as link funciton
gam_gl <- glm(y~X+0, inverse.gaussian(link = "1/mu^2"))  # fit effect is very good
summary(gam_gl)
sqrt(mean((coef(gam_gl)-beta)^2))

#-------------------------------
#install.packages('numDeriv')
library(numDeriv)
##  compute vector-independent variable scale-denpendent variable gradient
?grad
grad(sin, x= pi) # sin'(pi)=cos(pi)=-1
grad(sin, (0:10)*2*pi/10) # vectorize computing 
func0 <- function(x){ sum(sin(x))  } # user-defined function
grad(func0 , (0:10)*2*pi/10)
func0((0:10)*2*pi/10) # x\in R^n, y\in R^1
##  compute vector-independent variable vector-denpendent variable gradient
jacobian(Q2, rep(1,5))
?jacobian
func2 <- function(x) c(sin(x), cos(x))
x <- (0:1)*2*pi
jacobian(func2, x)
jacobian(func2, x, "complex")
## to calculate the Hessian (second derivative) of a scalar
#  real valued function with real n-vector argument.
?hessian
install.packages('globalOptTests')
library(globalOptTests)
??"globalOptTests"
library(help ='globalOptTests')

# Y-Bin(1, pi) Simulation -------------------------------------------------
x <- seq(-1.5, 1.5, by=0.001) 
b0 <- 1
b1 <- 2
tau0 <- 0 # 信息增强1000能把变点1.4探测出来 10000能把-1.3探测出来
b2 <- 1
z0 <- c(b0,b1,b2,tau0,gamma)
n <- length(x)
#gamma <- 1/n
x1 <- pmax(x-tau0, 0)
eta <- b0 + b1*x + b2*x1
u <- exp(eta)
y <- sapply(u, function(x) rpois(1, x))  # poisson 
summary(glm(y~x + x1, poisson(link='log')))
tau_seq <- seq(-1.5+0.05, 1.5-0.05, by = 0.1)
leng <- length(tau_seq)
Dev <- numeric(length=leng)
AIc <- Dev
for(i in 1:leng){
  x1_tau <- pmax(x-tau_seq[i], 0)
  poi_gl <- glm(y~x + x1_tau, poisson(link='log'))
  Dev[i] <- poi_gl$deviance
  AIc[i] <- poi_gl$aic
}
Dev
AIc
tau_seq
tau_seq[which.min(Dev)]
tau_seq[which.min(AIc)]
x1_tau <- pmax(x-tau_seq[which.min(AIc)], 0)
est_gl <- glm(y~x + x1_tau, poisson(link='log'))
summary(est_gl)



q <- function(x, tau=0, gam=0.1) (x-tau+gam)^2 / (4*gam) * (tau-gam<x & x< tau+gam) +
  (x > tau+gam)*(x -tau)

Eta <- function(x,b0,b1,b2,tau,gam) b0 + b1*x + b2*q(x, tau0, gamma)
Q <- function(x,y,b0,b1,b2,tau,gam) sum(log(1+exp(Eta(x,b0,b1,b2,tau,gam))) - 
                                          Eta(x,b0,b1,b2,tau,gam)*y)
Q(x,y,b0,b1,b2,tau0,gamma)
Q1 <-  function(b0,b1,b2,tau,gam) sum(log(1+exp(Eta(x,b0,b1,b2,tau,gam))) - 
                                            Eta(x,b0,b1,b2,tau,gam)*y)
Q1(b0,b1,b2,tau0,gamma)
Q2 <- function(z){
  b0 <- z[1]
  b1 <- z[2]
  b2 <- z[3]
  tau0 <- z[4]
  Q1(b0,b1,b2,tau0,gamma)
}
Q2(rep(0,5))
optim(rep(0,4), Q2, lower=c(rep(-2,3),-1.5), upper=c(rep(2,3), 1.5))
optim(c(b0,b1,b2,tau0), Q2, lower=c(rep(-2,3),-1.5), upper=c(rep(2,3), 1.5))

## LSE
b0 <- b1 <- 0.6
tau0 <- 0.2
b2 <- 1
z0 <- c(b0,b1,b2,tau0)
x <- seq(-1,1, by=0.005)
n <- length(x)
#n <- length(x1)
gamma <- 1/n
epsilon <- rnorm(n, sd=0.1)
q <- function(x, tau=0, gam=0.1) (x-tau+gam)^2 / (4*gam) * (tau-gam<x & x< tau+gam) +
  (x > tau+gam)*(x -tau)
eta <- function(x,b0,b1,b2,tau,gamma) b0 + b1*x + b2*q(x, tau0, gamma)
y <- eta(x,b0,b1,b2,tau0,gamma) +epsilon
Q <- function(theta){
  sum(( eta(x,theta[1],theta[2],theta[3],theta[4],gamma)^2/2 - 
          eta(x,theta[1],theta[2],theta[3],theta[4],gamma)*y))
}
Q(rep(1,4))
optim(rep(0.2,4), Q)
optim(rep(0.2,4), Q, lower=c(rep(-2,3),-1+0.2), upper=c(rep(2,3), 1-0.2)) # be influenced by initial points
c(b0,b1,b2,tau0)
