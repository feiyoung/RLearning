
# transfer any r.v. to a normal variable -------------------------------------------------------------------
set.seed(1234)
x <- rexp(100, 1/2)
xecdf <- ecdf(x)
xx <- qnorm(xecdf(x))
plot(density(xx))
shapiro.test(xx[-88])
x[which(xx==Inf)]

y <- 2*x+3
cov(x,y); cor(x,y)
plot(x, y, type='l')
yecdf <- ecdf(y)
yy <- qnorm(yecdf(y))
plot(density(yy))

cov(xx[-88],yy[-88]);cor(xx[-88],yy[-88])
plot(xx, yy, type='l')
is.infinite(xx)

# transfer multivariables into normal -------------------------------------

transMulVari <- function(x, stiInf='mean'){
  if(is.matrix(x) == F) x <- as.matrix(x)
  xx <- apply(x, 2, function(v){
    vecdf <- ecdf(v)
    vv <- qnorm(vecdf(v)) 
    ind <- which(vv==Inf)
    vv[ind]<- switch(stiInf,
    mean = mean(vv[-ind]),
    median = median(vv[-ind]),
    trimmed = mean(vv[-ind], trim=0.1)
    )
    return(vv)
  })
  cat('transfer is done!')
  return(xx)
}
z <- cbind(rnorm(100), rexp(100), rgamma(100, shape=0.5))
zz <- transMulVari(z)
cor(z) ;cor(zz) # 
