

# 快速生成对称矩阵的方法 -------------------------------------------------------------

P <- diag(0, nrow = d)
P[lower.tri(P)] <- param # param表示按列排列的下三角元素
P <- P + t(P)
diag(P) <- rep.int(1, d)
P



# 用再生核希尔伯特空间法做高维非参 --------------------------------------------------------
rkhs.reg <- function(x,y, kern, lambda){
  this.call <- match.call()
  n <- length(y)
  y <- as.double(y)
  x <- as.matrix(x)
  x <- cbind(matrix(1, n,1), x)
  Kmat <- kernelMatrix(kern, x)
  reg.coef <- solve(t(Kmat) %*% Kmat +lambda* Kmat) %*% t(Kmat)%*%y
  y.fit <- Kmat%*% reg.coef
  return(list(reg.coef=as.vector(reg.coef), y.fit=y.fit, lambda=lambda))
}
solve(Kmat)
#install.packages('KERE')
library(KERE)
# create data
N <- 200
X1 <- runif(N)
X2 <- 2*rnorm(N)
X3 <- 3*rexp(N)
SNR <- 10 # signal-to-noise ratio
Y <- X1*1.5 + 2 * (X2) + 2*X3
sigma <- sqrt(var(Y)/SNR)
y <- Y + rnorm(N,0,sigma)
x <- cbind(X1,X2,X3)

# set gaussian kernel 
kern <- rbfdot(sigma=0.01)

# define lambda sequence
lambda <- 0.2
#
rkhs.reg(x=x, y=y, kern=kern, lambda = lambda)

lambda <- exp(seq(log(0.5),log(0.01),len=10))

# run KERE
m1 <- KERE(x=X, y=Y, kern=kern, lambda = lambda, omega = 0.5) 

# plot the solution paths
plot(m1)

#
library(gam)
?gam
data(kyphosis)
head(kyphosis)
gam1 <- gam(Kyphosis ~ s(Age,4) + Number, family = binomial, data=kyphosis,
    trace=TRUE)
summary(gam1)
coefficients(gam1)
gam1$coefficients
gam1$residuals
mean(gam1$residuals)
gam1$additive.predictors
mean(gam1$additive.predictors)
gam1$fitted.values
1/(1 + exp(-gam1$additive.predictors)) # 用逆连接函数转换一下
gam1$smooth
gam1$nl.df
plot(kyphosis$Age, gam1$smooth[,1], type = 'p')


data(airquality)
gam(Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp), data=airquality, na=na.gam.replace)
gam(Kyphosis ~ poly(Age,2) + s(Start), data=kyphosis, family=binomial, subset=Number>2)
data(gam.data)
gam.object <- gam(y ~ s(x,6) + z,data=gam.data)
summary(gam.object)
plot(gam.object,se=TRUE)
data(gam.newdata)
predict(gam.object,type="terms",newdata=gam.newdata)
