#-----非参数线性回归的缺一交叉验证自定义函数

nonParLOOCV <- function(x, y, hRange, n, hvec, kernel='guass'){
# x: predictor variable
# y: response variable
# hRange: smoothing parameter limits, a vector with length 2
# n: number of given smoothing parameter
# hvec: a vector to specify the bandwidth
# kernel: kernel function, including 'guass','box', 'epan',
          #'triangle','biweight','triweight','tribube','cosinus'
 #x <- X;y <- Y;hRange=c(0.4,3); n=5
  if(missing(x) || missing(y) || (missing(hvec) && (missing(hRange) || missing(n)))){
    stop('Arguments are not enough!')
  }
  if(! missing(hvec)){
    n <- length(hvec)
    hvec <- hvec
  }else{
    n <- n
    hvec <- seq(hRange[1], hRange[2], length=n)
  }
  CV0 <- matrix(NA, nrow=n, ncol=2) # initiate CV matrix
  CV0[,1] <- hvec # give value to CV0
  j <- 1 # set first loop j
  while(j <= n){
    h <- CV0[j,1]
    #h <- 0.4
    RE <- 0
    for(i in 1:length(x)){
      x0 <- x[i]
      y0 <- y[i]
      yy <- y[-i]
      xx <- x[-i]
      #---------------begin choosing kernel
      #kern <- 0.75*(1-((xx-x0)/h)^2)/h*(abs(xx-x0)<=h) # epanchnnekov核函数
      kern <- if(any(kernel ==c('guass','guassian')))
        1/sqrt(2*pi)*exp(-(xx-x0)^2/h^2)/h # guassian kernel
      else if(any(kernel == c('4guass', 'fourguass','4guassian')))
        (3-(xx-x0)/h)^2/(2*sqrt(2*pi)) * exp(-((xx-x0)/h)^2/2) /h
      else if(kernel == 'box')
        1/2/h * (abs(xx-x0) <= h) # box kernel
      else if(kernel == 'epan')
        0.75*(1-((xx-x0)/h)^2)/h*(abs(xx-x0)<=h)
      else if(any(kernel == c('triangle','triangular')))
        (1-abs(xx-x0))/h * (abs(xx-x0)<=h)
      else if(kernel == 'biweight')
        15/16*(1-(xx-x0)^2)^2/h * (abs(xx-x0)<=h)
      else if(kernel == 'triweight')
        35/32*(1-(xx-x0)^2)^3/h * (abs(xx-x0)<=h)
      else if(kernel == 'tricube')
        70/81*(1-abs(xx-x0)^3)^3 * (abs(xx-x0)<=h)
      else if(any(kernel == c('cosinus','cos')))
        pi/4*cos(pi/2*(xx-x0))/h * (abs(xx-x0)<=h)
     #-----------------finish kernel setting
      #if(all(kern == 0)) 
      # stop('maybe exist outliner values')
      x1 <- xx-x0
      f=lm(yy~x1, weights=kern) # 带权的线性回归
      if(!is.na((y0-f$coefficient[1])^2)){ # omit NA values
      RE= RE+ (y0-f$coefficient[1])^2 
      }# 计算误差平方和
    }
    CV0[j, 2] <- RE
    #print(CV0[j,2])
    j <- j+1
  }
  plot(CV0[,1],CV0[,2],xlab="h",ylab="LOOCV",main="(a) h vs cv") # 会出图像
  lines(CV0[,1],CV0[,2])
  return(hvec[which.min(CV0[,2])])
}
data=read.csv("nonparadata1.csv") # 载入数据
Y=data[,1] 
X=data[,2]
opt.h <- nonParLOOCV(X, Y, hRange=c(0.1,1), n=20, kernel='cos') # choose optimal bandwidth
opt.h
