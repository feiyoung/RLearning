#---已完成的意思是已经复制到R帮助的word文件里面(当然是自己比较陌生且重要的代码)

rm(list=ls())
Path <- 'C:/Users/LiuWei/Desktop/探索R_matlab数据挖掘/'
setwd(Path)
getwd()

#-----------------
channel<-odbcConnectExcel(file.choose())  ##自己选择文件
channel  ##查看文件
sqlTables(channel)  ##列出表格
Data <- sqlFetch(channel,"Sheet1")
head(Data)

#-----1.
geneDf <- function(n){
v1 <- paste0('张三', 1:n)
v2 <- sample(1:80, n, replace=T)
v3 <- sample(c('男','女'), n, replace=T)
v4 <- as.numeric(paste0(13637894, sample(0:9, n, replace=T),sample(0:9, n,replace=T),
                        sample(0:9, n,rep=T)))
return(data.frame(姓名=v1, 年龄=v2, 性别=v3, 联系方式=v4))
}
set.seed(1234)
(Data <- geneDf(10))
options(digits=12)
Data <- read.csv('rstudio 数据.csv')# R支持纯中文变量名，好屌
subset(Data, 性别=='男' & 电话号码 %%2 ==0)

#-----2.
options(digits=3)
install.packages('plyr')
library(plyr)
data(mtcars)
?mtcars
V1 <- aggregate(mpg~ cyl, mtcars, min)[,2]
set1 <- subset(mtcars, mpg==V1)
rownames(set1)
V2 <- aggregate(mpg~ am, mtcars, min)[,2]
set2 <- subset(mtcars, mpg==V2[1] | mpg==V2[2])
mtcars[mtcars$mpg==V2[2],]
rownames(set2)
aggregate(mpg~ cyl, mtcars, mean)
aggregate(mpg~ am, mtcars, mean)


#------------------
library(caret)
data(BloodBrain) # BloodBrain include a data.frame bbbDescr and a vector logBBB
## sample the train dataset
inTrain <- createDataPartition(logBBB, p = .8)[[1]]
trainX <- bbbDescr[inTrain,] 
trainY <- logBBB[inTrain]
## the reminder as the test dataset
testX <- bbbDescr[-inTrain,]
testY <- logBBB[-inTrain]


fit <- knnreg(trainX, trainY, k = 3) # create the kNN regression model 
pred <- predict(fit, testX) # predict the new data
plot(testY, pred)
#我的程序：
data634 <- read.csv('634.csv', header=T)
set.seed(1234)
index<-sample(1:nrow(data634),as.numeric(0.7*nrow(data634)));index 
trainy <- data634[index,18] 
trainx<- data634[index,-18] 
testx <- data634[-index,-18] 
testy <- data634[-index,18]
fit<-knnreg(trainx, trainy, k=6)
pred <- predict(fit, testx) # predict the new data
plot(testy, pred)
Fun <- function(k){
  fit<-knnreg(x=trainx, y=trainy,k)
  pred <- predict(fit, testx) # predict the new data
  sum((pred-testy)^2) / sum((testy-mean(testy))^2) 
}
Fun(k=6)
K <- 1:20
plot(K,sapply(K, Fun), type='b', ylab='NMSE')
## mean absolute error(MAE)
mean(abs(pred-testy))
## mean squared error(MSE)
mean((pred-testy)^2)
## root of mean squared error(RMSE)
sqrt(mean((pred-testy)^2))
##normalised mean squared error(NMSE)
sum((pred-testy)^2) / sum((testy-mean(testy))^2) 

regr.eval(testy, pred, train.y= data634[index,18])
#---------------help 2015.7.1
load('d21.Rdata')
install.packages('lubridate')
library(lubridate)
newdata <- d21
newdata$years1 <- year(d21$creation_ts)
d1998 <- d21[newdata$years1==1998,]
d1998 <- d21[newdata$years1==1998 & newdata$reporter=='bugzilla',]
d1998 <- subset(newdata,years1==1998 & reporter=='bugzilla')[,-12]
(fixed1998 <- summary(d1998$resolution)['FIXED'])

aggregate(x = d21, by = list(newdata$years1), FUN = "summary")
#--------------------help the barfriend
rmix<-function(n,mu=0, k=1, sd=1, p=0.5){
  n1 <- rnorm(n,mu,sd)
  n2 <- rnorm(n,mu,k*sd)
  mix <- as.integer(runif(n)<p)
  return(mix*n1 + (1-mix)*n2)
}
rmix(100)

#----------help the bar 2015.6.26
install.packages('multcomp')
library(multcomp)
par(mar=c(5,4,6,2),las=2)
tuk<-glht(fit,linfct=mcp(trt="Tukey"))
plot(cld(tuk,level=0.05),col="lightgrey")

#---------------------
data1 <- read.table("2012.txt", header = T)
data2 <- read.table("05到12.txt", header=T)
colnames(data2) <- c('year','accident','death','injured','loss')
str(data1)
str(data2)
summary(data1)
summary(data2)
#---------corrlation analysis
(cor2 <- cor(data2[,-1]))
#------- visualize the correlation
symnum(cor2)
install.packages('ggplot2')
library(ggplot2)
p <- ggplot(data2, mapping=aes(x=year, y=accident))
p + geom_line()
# number of accidents is decreasing with increament of year
p + geom_line(aes(colour= year)) 
p + geom_point(aes(x= accident, y=death, colour=year)) +
  xlab('accident') + ylab('death')
p + geom_line(aes(x= accident, y=injured)) +
  xlab('accident') + ylab('injured')
p + geom_line(aes(x= accident, y=loss)) +
  xlab('accident') + ylab('loss')
p + geom_boxplot()
p1 <- ggplot(data2, aes(x=accident))
p1 + geom_histogram( binwidth=2)

#---------linear regression 
ad.lm <- lm(death~accident, data=data2)
summary(ad.lm)
oldPar <- par(mfrow=c(2,2))
plot(ad.lm)
par(oldPar)
al.lm <- lm(loss~accident, data=data2)
summary(al.lm)
oldPar <- par(mfrow=c(2,2))
plot(al.lm)
par(oldPar)



#------------帮助贴吧2015.6.24
x1<-c(82.9,88,99.9,105.3,117.7,131,148.2,161.8,174.2,184.7)
x2<-c(92,93,96,94,100,101,105,112,112,112)
x3<-c(17.1,21.3,25.1,29,34,40,44,49,51,53)
x4<-c(94,96,97,97,100,101,104,109,111,111)
y<-c(8.4,9.6,10.4,11.4,12.2,14.2,15.8,17.9,19.6,20.8)
x<-cbind(x1,x2,x3,x4)
xx<-crossprod(x)
kappa(xx,exact=T)
plot(lm.ridge(y~x1+x2+x3+x4,lambda=seq(0,0.5,0.001)))
y.pr<-princomp(~x1+x2+x3+x4,cor=T)
summary(y.pr,loadings=T)
#importance of components
z1<-predict(y.pr)[,1]
lm.sol<-lm(y~z1)
summary(lm.sol)
beta<-coef(lm.sol);A<-loadings(y.pr)
x.bar<-y.pr$center;x.sd<-y.pr$scale
coef<-(beta[2]*A[,1])/x.sd
beta0<-beta[1]-sum(x.bar*coef)
c(beta0,coef)



#---------------距离判别函数
dist.da <- function(data, newdata=NULL){
  data <- data1; newdata <- newdata
  mn <- dim(data)
  n <- mn[2]-1
  if(!is.data.frame(data)) stop('data must be a data.frame')
  if(is.null(newdata)) newdata <- data[,1:n]
  if(is.vector(newdata)) newdata <- t(newdata) 
  else if(! is.matrix(newdata)) newdata <- as.matrix(newdata)
  class1 <- levels(data[,mn[2]])
  K <- nlevels(data[,mn[2]])
  T <- lapply(class1, function(x) subset(data[,1:n],data[,mn[2]]==x))
  Center <- lapply(T, colMeans)
  Cov <- lapply(T, cov) 
  
  ##对待分类样本进行判别分类
  leiBie<-0
  for(i in 1:dim(newdata)[1]){
    R <- NULL
    for(j in 1:K){
      R[j]<-mahalanobis(newdata[i,],center=Center[[j]],cov=Cov[[j]])
    }
    leiBie[i] <- class1[which.min(R)[1]]
  }
  leiBie #输出分类结果
}
dist.da(data1, newdata)

#--------help the bar(2015.6.23)
#---load the data
X1 <- c(-1.9,-6.9,5.2,5.0,7.3,6.8,0.9,-12.5,1.5,3.8,
        0.2,-0.1,0.4,2.7,2.1,-4.6,-1.7,-2.6,2.6,-2.8)
X2 <- c(3.2,10.4,2.0,2.5,0.0,12.7,-15.4,-2.5,1.3,6.8,
        0.2,7.5,14.6,8.3,0.8,4.3,10.9,13.1,12.8,10.0)
n <- length(X1)/2
Wea <- c(rep('rain', n),rep('sun', n))
data1 <- data.frame(X1=X1,X2=X2, Weather=Wea)
newdata <- data.frame(X1=8.1,X2=2.0)
#--distance to discriminating analysis
T1<-subset(data1[,1:2],data1[,3]=='rain') 
T2<-subset(data1[,1:2],data1[,3]=='sun') 
CenterT1<-colMeans(T1) #按列计算均值
CenterT2<-colMeans(T2) #T2的列均值
Center<-rbind(CenterT1,CenterT2) #按行合并
Cov1<-cov(T1);Cov2<-cov(T2)
leiBie<-0
for(i in 1:dim(newdata)[1]){
  R1<-mahalanobis(newdata[i,1:2],center=CenterT1,cov=Cov1)
  R2<-mahalanobis(newdata[i,1:2],center=CenterT2,cov=Cov2)
  if(R1<R2 )  leiBie[i]<-'rain'
  if(R2<R1 ) leiBie[i]<- 'sun'
}
leiBie #输出分类结果
#-------Beysian discriminating anlysis
discriminiant.bayes <- function(data1, testdata=NULL, rate = 1, 
                                var.equal = FALSE){
  if(is.null(testdata)) testdata <- data1[,1:(length(data1)-1)]
  if(is.vector(testdata)) testdata <- t(testdata)
  if (is.data.frame(data1) != TRUE) 
    stop('the data must be a dataframe')
  m <- dim(testdata)[1]; n<- dim(testdata)[2]
  result <- character(m)
  wea1 <- data1[,n+1]
  class1 <- levels(wea1)
  TrainX1 <- data1[wea1==class1[1],]
  TrainX2 <- data1[wea1==class1[2],]
  TrnX1 <- TrainX1[,1:n]
  TrnX2 <- TrainX2[,1:n]
  mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
  if (var.equal == TRUE || var.equal == T){
    S <- var(rbind(TrnX1,TrnX2)); beta <- 2*log(rate)
    w <- mahalanobis(testdata, mu2, S)
    - mahalanobis(testdata, mu1, S)
  }
  else{
    S1 <- var(TrnX1); S2 <- var(TrnX2)
    beta <- 2*log(rate) + log(det(S1)/det(S2))
    w <- mahalanobis(testdata, mu2, S2)
    - mahalanobis(testdata, mu1, S1)
  }
  for (i in 1:m){
    if (w[i] > beta)
      result[i] <- class1[1]
    else
      result[i] <- class1[2]
  }
  result
}
## when the variance are equal
discriminiant.bayes(data1,testdata=newdata, rate=8/6,var.equal=TRUE)
## when the variance are not equal
discriminiant.bayes(data1,testdata=newdata, rate=8/6)

#-------Fisher discriminating analysis
library(MASS)
wea.lda<-lda(Weather~., data=data1)
wea.lda
(wea.pred<-predict(wea.lda, newdata[,1:2])$class)

#---------------help the baidubar
aa <- function(x,mu,sigma){
  y <- NULL
  for(i in 1:length(x)){
    a <- sqrt(2*pi)*sigma
    y[i] <- 1/a*exp(-(x[i]-mu)^2/ (2*sigma^2))
  }
  a <- 0
  for(j in 1:length(y)) {a <- a + log(y[j])}
  a
}

LogLike <- function(x, mu, sigma){
  a <- sqrt(2*pi)*sigma
  y <- sapply(x, function(x1) 1/a *exp(-(x1-mu)^2/ (2*sigma^2)) )
  sum(log(y))
}
mu <- 0;sigma<-3
set.seed(123)
X <- rnorm(10, mu, sigma)
aa(X, mu, sigma)
LogLike(X, mu, sigma)
#--------item (2)
f <- function(x, theta, eta) 1/(theta*pi*(1+((x-eta)/theta)^2))
m <- 5000
theta <- 1; eta<- 0
x <- numeric(m)
x[1] <- rnorm(1)
k <- 0
u <- runif(m)
for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1, mean = xt)
  num <- f(y,theta, eta) * dnorm(xt, mean = y)
  den <- f(xt, theta, eta) * dnorm(y, mean = xt)
  if (u[i] <= num/den) x[i] <- y else {
    x[i] <- xt
    k <- k+1 #y is rejected
  }
}

index <- 1001:m
y1 <- x[index]
#----------output the simulated value
y1
#-----------compare the result with the true random value
plot(index, y1, type="l", main="", ylab="x")
mm <- length(index)
set.seed(1234)
y2 <- rcauchy(mm, location = 0, scale = 1)
plot(index, y2, type="l", main="", ylab="x")

#帮助贴吧
## get the leap year
isLeapday<-function(x)
{
  a <- x %% 4
  b <- x %% 100
  c <- x %% 400
  if((a ==0 & b != 0) | c ==0){
    return('this year is  leap year')} else{
      return('this year is not  leap year')
    }
  
}
X <- 2000
isLeapday(X)
XX <- c(2000, 2004, 2008, 2009)
sapply(XX, isLeapday)
##帮助贴吧 get the contour plot
library(MASS)
library(fields)
fmixture <- function(x, p1) {
  p1 * dnorm(x,0,1) + (1-p1)*dnorm(x, 3, sd=sqrt(2))
} 
MCMCsample <- function(N,...){
  m <- 1000+N
  x <- numeric(m)
  x[1] <- rnorm(1)
  k <- 0
  u <- runif(m)
  for (i in 2:m) {
    xt <- x[i-1]
    y <- rnorm(1, mean = xt)
    num <- fmixture(y, p1) * dnorm(xt, mean = y)
    den <- fmixture(xt, p1) * dnorm(y, mean = xt)
    if (u[i] <= num/den) x[i] <- y else {
      x[i] <- xt
      k <- k+1 #y is rejected
    }
  }
  
  index <- 1001:m
  x[index]
}
p1 <- 0.2
N <- 1000
X <- MCMCsample(N)
Y <- MCMCsample(N)
#---plot
z <- kde2d(X,Y, n=30)
contour(z, col = 'red', drawlabel=T, main="Density estimation :cont Plot")
win.graph()
filled.contour(z, color = terrain.colors, plot.title = title(main='filled  contour'),
               key.title = title(main='density'))
win.graph()
plot.surface( z)
#------------

x <- rnorm(1000, 0, 1)
y <- rnorm(1000, 1, 100)
z1 <- cbind(x, y)
x <-  1:nrow(z1)
y <-  1:ncol(z1)
filled.contour(x, y, z1, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               key.title = title(main = "Height\n(meters)"))
plot (x, y)
z <- kde2d(x,y)
contour(z, col = 'red', drawlabel=FALSE, main="Density estimation :cont Plot")
persp(z, main = "Density estimation:perspective plot") # 透视图
filled.contour(z, color = terrain.colors, plot.title = title(main='contour plot'),
               xlab='X variable', ylab='Y variable',key.title = title(main='Height'))
