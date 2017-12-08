
#--my own function
# 1. use MM Algorithm to evaluate sample median ------------------------------


mymedian <- function(x, theta0, tol = .Machine$double.eps^0.25, maxtiter=1000){
# MM algorithm to evaluate median
# the result is related to initial theta
# # generate data to test the function
# x <- 1:3
# theta <- 1.2
# f(x, theta)
# theta0 <- theta
# tol = .Machine$double.eps^0.25
k <- 0
  f <- function(x, theta){
    sum(abs(x-theta))
  }
epsilon <- 1e-8
f1 <- f(x, theta0)
f2 <- Inf
  while(abs(f2-f1) > tol){
    f1 <- f(x, theta0)
    temp <- abs(x-theta0)
    temp[which(temp==0)] <- epsilon
    up <- sum(x/temp)
    down <- sum(1/temp)
    theta0 <- up/down
    f2 <- f(x, theta0)
    k <- k + 1
    if(k > maxtiter) break
  }
return(theta0)
}
x <- 1:3
mymedian(x, theta0 = 0, maxtiter = 4) # convergent rate is very fast!
median(x)


# 2. estimating teams' skill level ----------------------------------------
# data generating
# 2维的情况
rm(list=ls())
b <- cbind(c(0,1), c(2,0)) # games defeat cases
class(b)
n <- nrow(b)
theta0 <- matrix(1:n,1)
norm(theta0, type='2')
theta1 <- matrix(rep(Inf,n),1)
C <- matrix(NA, n, n) -> e
k <- 0
while(norm(theta1-theta0, 'I') > 1e-2){
  theta1 <- theta0
  for(i in 1:n){
    for(j in 1:n){
      C[i,j] <- 1/(theta0[i] + theta0[j])
      e[i,j] <- -b[i,j] * C[i,j]
    }
  }
  f <- apply(e, 1, sum)
  g <- apply(e, 2, sum)
  d <- apply(b, 2, sum)
  theta0 <- matrix(- d/(f+g),1)
  theta0[1,1] <- 1
  k <- k+1
}
theta0

# 3维情况
rm(list=ls())
b <- cbind(c(0,4,6), c(1,0,7), c(0,1,0)) # games defeat cases
class(b)
n <- nrow(b)
theta0 <- matrix(1:3,1)
theta1 <- matrix(rep(Inf,3),1)
norm(theta1-theta0, 'I')
C <- matrix(NA, n,n) -> e
k <- 0
while(norm(theta1-theta0, 'I') > 1e-5){
  theta1 <- theta0
  for(i in 1:n){
    for(j in 1:n){
      C[i,j] <- 1/(theta0[i] + theta0[j])
      e[i,j] <- -b[i,j] * C[i,j]
    }
  }
  f <- apply(e, 1, sum)
  g <- apply(e, 2, sum)
  d <- apply(b, 2, sum)
  theta0 <- matrix(- d/(f+g),1)
  theta0[1,1] <- 1
  k <- k+1
}
theta0

