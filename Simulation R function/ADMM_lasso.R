#---------------ADMM algorithm to computation
rm(list=ls())
lasso_admm <- function(A, b, lambda, rho, alpha){
  require(Matrix)
  options(digits=3)
  # my own function
  # 1.
  factor <- function(A, rho){
    m <- nrow(A)
    n <- ncol(A)
    if(m >= n){
      R <- chol(t(A) %*% A + rho*Diagonal(n))
    }else{
      R <- chol(Diagonal(m)+1/rho*(A%*%t(A)))
    }
    return(Matrix(t(R), sparse = T))
  }
  # 2.
  shrinkage <- function(x, kappa){
    x <- as.vector(x)
    z <- pmax(0, x-kappa) - pmax(0, -x-kappa)
    return(as(z, "sparseMatrix"))
  }

  # 3. 
  objective <- function(A, b, lambda, x, z){
    p <- (1/2 * sum((A %*% x- b)^2)) + lambda * sum(abs(z))
    return(p)
  }
  # 4.
  normMe <- function(x, ...) round(Matrix::norm(x, type='f'), ...)
  #main code 
  QUIET <- 0
  MAX_ITER <- 1000
  ABSTOL <- 1e-4
  RELTOL <- 1e-2
  # Data preprocessing
  m <- nrow(A)
  n <- ncol(A)
  Atb <- t(A) %*% b
  # ADMM solver
  x <- sparseMatrix(i=1:n, j=rep(1, n), dims=c(n,1), x= 0) # initialize with sparse mat
  z <- x
  u <- x
  # cache the factorization
  L <- factor(A, rho)
  U <- t(L)
  
  #
  if(! QUIET){
    cat('iter', 'r norm','eps pri','  s norm', '  eps dual','objective\n', sep='\t')
  }
  # k <- 1
  for(k in 1:MAX_ITER){
    # x-update
    q <- Atb + rho*(z - u)
    if(m >= n){
      x <- solve(U)%*%solve(L)%*% q
    }else{
      x <- q/rho - (t(A) %*% (solve(U)%*%(solve(L)%*% (A%*% q))))
    }
    # z-update
    zold <- z
    x_hat <- alpha*x + (1-alpha)*zold
    z <- shrinkage(x_hat+u, lambda/rho)
    # u-update
    u <- u + (x_hat-z)
    # diagnotics, reporting, termination checks
    history <- list() # it must initialized
    history$objval[k] <- round(objective(A, b, lambda, x, z),3)
    history$r_norm[k] <- normMe(x-z, digits=3)
    history$s_norm[k] <- normMe(-rho*(z-zold), digits=3)
    history$eps_pri[k] <- sqrt(n) *ABSTOL + RELTOL*max(normMe(x, digits=3), normMe(-z, digits=3))
    history$eps_dual[k] <- sqrt(n)*ABSTOL + RELTOL*normMe(rho * u, digits=3)
    if(! QUIET){
      cat(k, history$r_norm[k],history$eps_pri[k],history$s_norm[k],
          history$eps_dual[k],history$objval[k],'\n', sep='  \t')
    }
    if(history$r_norm[k]< history$eps_pri[k] && history$s_norm[k] < history$eps_dual[k]) 
      break
  }
  return(list(z, history))
}
library(Matrix)
detach(package:Matrix)
#library(help ='Matrix')
set.seed(1234)
m <- 150
n <- 500
p <- 10/n
x0 <- sparseMatrix(i=1:10,j= rep(1, n*p), x=rnorm(n*p), dims=c(n,1))
x0[1:10]
A <- Matrix(data=rnorm(m*n), m, n)
A <- A%*%Diagonal(x=1/sqrt(apply(A^2,2,sum)))
dim(x0)
dim(A)
b <- A%*%x0 + sqrt(0.001)*Matrix(data=rnorm(m,1),m, 1) 

lambda_max <- norm(t(A) %*% b, 'i')
lambda <- 0.1*lambda_max
# ADMM
rho <- 1
alpha <- 0.5
Res <- lasso_admm(A, b, lambda= 0.2,rho, alpha )
Res[[1]][1:10]
which(Res[[1]] != 0)
which(c(as.vector(x0)) !=0)
x0[1:10]

library(glmnet)

# just use the coordinate decent algorithm to solve it !
res <- glmnet(A, b, intercept = T)
which(coef(res, s=0.02) != 0)
which(c(2,as.vector(x0)) !=0) # lasso could find all the nonzero point, I admire it!



# 系数矩阵包Matrix的使用 ----------------------------------------------------------


library('Matrix')

m1 <- matrix(0, nrow = 1000, ncol = 1000)
m2 <- Matrix(0, nrow = 1000, ncol = 1000, sparse = TRUE)
object.size(Diagonal(1000))
object.size(m1)
# 8000200 bytes
object.size(m2)
# convert a ordinary matrix to sparse matrix
C <- as(L, "sparseMatrix")       # see also `vignette("Intro2Matrix")`
B <- Matrix(L, sparse = TRUE)    # Thanks to Aaron for pointing this out