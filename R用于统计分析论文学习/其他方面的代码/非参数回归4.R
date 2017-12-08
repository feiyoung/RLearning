#-----非参数线性回归的K-折交叉验证函数

nonParKCV <- function(x, y, hRange, n, hvec, K, seed, kernel='guass'){ 
  # x: predictor variable
  # y: response variable
  # hRange: smoothing parameter limits, a vector with length 2
  # n: number of given smoothing parameter
  # hvec: a vector to specify the bandwidth
  # k: k-fold cv
   #x <- X;y <- Y;hRange=c(0.4,3); n=5; K=2; seed=1
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
  #--data grouping 
  N <- length(x)
  gn <- floor(N/K)
  set.seed(seed)
  inde <- sample(1:N) # random grouping
  findex <- inde[((K-1)*gn+1):N] # draw the final data's index as test data
  
  CV0 <- matrix(NA, nrow=n, ncol=2)
  CV0[,1] <- hvec
  j <- 1
  while(j <= n){
    h <- CV0[j,1]
    #h <- 0.4
    RE <- 0
    for(i in 1:K){ # K-fold cross validation
      if(i != K){
        ind1 <- (1:gn) + (i-1)*gn
        index <- inde[ind1]
      }else{
        index <- findex
      }
      x0 <- x[index]
      y0 <- y[index]
      yy <- y[-index]
      xx <- x[-index]
      sum1 <- 0
      for(k in 1:length(index))
      {
        #---------------begin choosing kernel
        #kern <- 0.75*(1-((xx-x0)/h)^2)/h*(abs(xx-x0)<=h) # epanchnnekov核函数
        kern <- if(any(kernel ==c('guass','guassian')))
          1/sqrt(2*pi)*exp(-(xx-x0[k])^2/h^2)/h # guassian kernel
        else if(any(kernel == c('4guass', 'fourguass','4guassian')))
          (3-(xx-x0[k])/h)^2/(2*sqrt(2*pi)) * exp(-((xx-x0[k])/h)^2/2) /h
        else if(kernel == 'box')
          1/2/h * (abs(xx-x0[k]) <= h) # box kernel
        else if(kernel == 'epan')
          0.75*(1-((xx-x0[k])/h)^2)/h*(abs(xx-x0[k])<=h)
        else if(any(kernel == c('triangle','triangular')))
          (1-abs(xx-x0[k]))/h * (abs(xx-x0)<=h)
        else if(kernel == 'biweight')
          15/16*(1-(xx-x0[k])^2)^2/h * (abs(xx-x0[k])<=h)
        else if(kernel == 'triweight')
          35/32*(1-(xx-x0[k])^2)^3/h * (abs(xx-x0[k])<=h)
        else if(kernel == 'tricube')
          70/81*(1-abs(xx-x0[k])^3)^3 * (abs(xx-x0[k])<=h)
        else if(any(kernel == c('cosinus','cos')))
          pi/4*cos(pi/2*(xx-x0[k]))/h * (abs(xx-x0[k])<=h)
        #-----------------finish kernel setting
        x1 <- xx- x0[k]
        f=lm(yy~x1, weights=kern) # 带权的线性回归
        if(!is.na((y0[k]-f$coefficient[1])^2)){
        sum1=sum1+(y0[k]-f$coefficient[1])^2 # 计算误差平方和
        }
      }
   RE=RE+sum1 # 得到误差平方和
    }
    CV0[j, 2] <- RE
    print(CV0[j,2])
    j <- j+1
  }
  plot(CV0[,1],CV0[,2],xlab="h",ylab="cv",main="(a) h vs KCV") # 会出图像
  lines(CV0[,1],CV0[,2])
  return(hvec[which.min(CV0[,2])])
}

opt.h <- nonParKCV(X, Y, hRange=c(0.1,1), n=8, K=10, seed=1234, kernel='triweight')
opt.h
