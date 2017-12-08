setwd('C:\\Users\\Administrator\\Desktop\\新建文件夹\\非参数回归')
getwd()
rm(list=ls())
library(MASS)
library(KernSmooth)
library(locfit)
#library(help = 'KernSmooth')
#library(help ='locfit')
save.image('nonPara_Regre.Rdata')
load('nonPara_Regre.Rdata')
Data <- read.csv('C:\\Users\\Administrator\\Desktop\\新建文件夹\\非参数回归\\nonparadata1.csv')
par(mfrow= c(1,1))
X <- Data[,2]
Y <- Data[,1]
plot(X,Y, type='p', col='blue', main='raw data plot')


#------------------局部回归
#---------回归估计
h1 <- dpik(X, kernel = 'epanech') # 选用epanech核，利用直接插入法选择核密度估计
h1
par(mfrow=c(2,2))
est<-locpoly(X,Y, bandwidth=h1/10, kernel = 'epanech') #local polnomial regression
plot(X,Y,xlab="X",ylab="Y", type='p')  # visibilize
lines(est,col=2) 
plot(est,type="l",xlab="X",ylab=expression(hat(Y)),col=1) 
est<-locpoly(X,bandwidth=90) 
plot(est,type="l",xlab="",ylab="density",col=3) 
est<-locpoly(X,bandwidth=1000) # 多项式密度估计
plot(est,type="l",xlab="",ylab="density",col=3) 


#-----------------不同窗宽下局部多项式的非参数回归模型

par(mfrow=c(2,2)) 
est1<-locpoly(X,Y,degree=25,bandwidth=2) # 25次多项式
plot(X,Y,xlab="",ylab="") 
lines(est1,col=1) 
est2<-locpoly(X,Y,degree=20, bandwidth=3) 
plot(X,Y,xlab="",ylab="") 
lines(est2,col=2) 
est3<-locpoly(X,Y,degree=15, bandwidth=4) 
plot(X,Y,xlab="",ylab="") 
lines(est3,col=3) 
est4<-locpoly(X,Y,degree=10,bandwidth=5) #10次多项式效果更好
plot(X,Y,xlab="",ylab="") 
lines(est4,col=4) 

#----------相同窗宽下当局部多项式的阶数P值不同时的非参数回归模型
h2 <- dpill(X, Y) # 选用normal核，利用直接插入法选择窗宽
par(mfrow=c(2,2)) 
plot(X,Y,xlab="",ylab="") 
lines(locpoly(X,Y,drv=0L,degree=0, bandwidth=h2),col=1) #degree=0为局部常数
plot(X,Y,xlab="",ylab="") 
lines(locpoly(X,Y,drv=0L,degree=1,bandwidth=h2),col=2) # degree为局部线性
plot(X,Y,xlab="",ylab="") 
lines(locpoly(X,Y,drv=0L,degree=3,bandwidth=h2),col=3) #3次多项式
plot(X,Y,xlab="",ylab="") 
lines(locpoly(X,Y,drv=0L,degree=0, bandwidth=h2),col=1) 
lines(locpoly(X,Y,drv=0L,degree=1,bandwidth=h2),col=2) 
lines(locpoly(X,Y,drv=0L,degree=3,bandwidth=h2),col=3) 

h3 <- dpill(X,Y) # 选用normal核，利用直接插入法选择窗宽
h3
par(mfrow=c(1,2)) 
plot(X,Y,xlab="",ylab="", main='p = 0:局部常数') 
lines(locpoly(X,Y,drv=0L,degree=0, bandwidth=h3),col=1) #degree=0为局部常数
plot(X,Y,xlab="",ylab="", main='p = 1：局部线性') 
lines(locpoly(X,Y,drv=0L,degree=1,bandwidth=h3),col=2) # degree为局部线性

plot(X,Y,xlab="",ylab="", main='p = 3') 
lines(locpoly(X,Y,drv=0L,degree=3,bandwidth=h3),col=3) 
plot(X,Y,xlab="",ylab="") 
lines(locpoly(X,Y,drv=0L,degree=0, bandwidth=h3),col=1) 
lines(locpoly(X,Y,drv=0L,degree=1,bandwidth=h3),col=2) 
lines(locpoly(X,Y,drv=0L,degree=3,bandwidth=h3),col=3) 
legend("bottomright",c("p=0","p=1", 'p=3'),col=c('black','red','green'),
       lty=1, bg="white",cex=0.5)


#install.packages('locfit') #局部多项式回归包
library(locfit)
gcv.fit1 = c(0)
b = seq(from = 0.1, to = 1, by = 0.01) 
for(i in 1:length(b)) 
{ 
  fit1 = locfit(Y~lp(X, nn = b[i])) #利用默认的tricubic核        
  gcv.fit1[i] =  gcv(fit1)[4]  # 得到广义交叉验证得分
} 
gcv.fit1 = round(gcv.fit1,4) 
b1 = b[min(which.min(gcv.fit1))] 
fit1 = locfit(Y~lp(X, nn = b1))
pdf("tricubic_fit2.pdf",width=7,height=5)
plot(X,Y, type='p', col='blue', main='raw  vs estimation')
lines(fit1, pch=2, col='red')
dev.off()
pdf("tricubic_GCV2.pdf",width=7,height=5)
plot(b,gcv.fit1 ,xaxt="n", type='b',
     main='tricubic kernel', ylab='GCv score', xlab='h')
axis(side=1,at=seq(from = 0.1, to = 1.0, by = 0.1))   
dev.off()



#------------------用Epanechnic核进行广义交叉验证
gcv.fit = c(0)
b = seq(from = 0.1, to = 2, by = 0.01) 
for(i in 1:length(b)) 
{ 
  fit = locfit(Y~lp(X, nn=0.02, h = b[i]), kern='epan')  
  #fit = locfit.raw(X, Y, kern='epan', alpha=b[i])        
  gcv.fit[i] =  gcv(fit)[4] 
} 
gcv.fit = round(gcv.fit,4) 
b2 = b[min(which.min(gcv.fit))] 
fit = locfit(Y~lp(X, nn = 0.02, h=b2)) 

gcv.fit = c(0)
b = seq(from = 0.1, to = 2, by = 0.01) 
for(i in 1:length(b)) 
{ 
  fit = locfit(Y~lp(X, nn=b[i]), kern='bisq')  #原来平滑参数的大小和核函数的选择基本无关，都是0.24
  #fit = locfit.raw(X, Y, kern='epan', alpha=b[i])        
  gcv.fit[i] =  gcv(fit)[4] 
} 
gcv.fit = round(gcv.fit,4) 
(b2 = b[min(which.min(gcv.fit))] )
fit = locfit(Y~lp(X, nn = b2)) 
#----输出图像到pdf中
pdf("epan_fit2.pdf",width=7,height=5)
plot(X,Y, type='p', col='blue', main='raw  vs estimation')
lines(fit, pch=2, col='red')
dev.off()
#legend("bottomright",c('raw', 'estimation'),pch=1:2, col=c('blue','red'),bg="white",cex=0.5)
#plot(b, gcv.fit, main='Eanechnikov kernel', ylab='GCv score', xlab='h')
pdf('epan_GCV.pdf', width=7, height=5)
plot(b,gcv.fit ,xaxt="n", type='b',
     main='Eanechnikov kernel', ylab='GCv score', xlab='h')
axis(side=1,at=seq(from = 0.1, to = 2, by = 0.2))   
dev.off()
#--采用其他平滑参数值
pdf("tricubic_varous_Par.pdf",width=7,height=5)
plot(X,Y, type='p', col=4, main='raw  vs estimation')
lines(locfit(Y~lp(X, nn = 0.02, h=b2)),  col=2)
lines(locfit(Y~lp(X, nn = 0.02, h=0.1)), col=3)
lines(locfit(Y~lp(X, nn = 0.02, h=10)), col=5)
#legend("bottomright",c('h=GCV value', 'h=0.1','h=10'),
      # pch=1, col=2:4,bg="white",cex=0.5)
legend("bottomright", c('h=GCV value', 'h=0.1','h=10'), cex = 0.8, 
       lty = 1,  col = c(2:3,5), inset = 0.01, box.col = "white")
dev.off()