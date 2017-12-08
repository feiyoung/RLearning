##-创建R包了

rm(list=ls())
#
MonteCarloPi <- function(nsim){
  x <- 2*runif(nsim)-1
  y <- 2*runif(nsim)-1
  inx <- which((x^2+y^2)<=1)
  return ((length(inx)/nsim)*4)
}
clime <- function(data, ...){
  require('fastclime')
  out1 = fastclime(data,...)
  return(out1)
}
f1 <- function(aa) {return(aa)}
f2 <- function(aa) {return(aa+2)}
.sum <- function(aa=1, bb= 1) {return(aa + bb)}
d1 <- matrix(1:6, 2, 3)
data(stockdata)
mystock <- stockdata$data[,1:20]
# name denote the name of my package
# list is character vector naming the R objects to put in the package
package.skeleton(name='test1', list=c('MonteCarloPi','f1','f2',
                                      'clime','.sum', 'd1','mystock'))
install.packages("D:/学习/R语言代码/test1_1.0.tar.gz", repos = NULL, type = "source")
library(test1)
library(help = 'test1')
rm(list=ls())
MonteCarloPi(100)
?MonteCarloPi
remove.packages('test1')
data(d1)
?f1
example(f1)
data(mystock)
m <- clime(mystock)
search() # view packages loaded in, including fastclime!
detach('package:fastclime') # unload the fastclime package and do following command
m <- clime(mystock) # the fastclime package is loaded again quietly!

# My own convenient function ----------------------------------------------

##report process for fixe loop number(such as for loop)
disProBar <- function(k, NN){
# k is current loop number 
# NN is total loop number
  if(k%%(NN/10)==0){
    print(paste('==============程序运行进度为： ',10*floor(k/(NN/10)),'% =============', sep = ''))
  }
}
s <- 0
N <- 1000000
for(i in 1:N){
  disProBar(i, N)
  s <- s+i 
} 
#-------与C交互
x = rnorm(30,0,1);
UseData = x + c( 0, .3 * x[1:29]) + c(0,0, .1 * x[1:28]); ## Creates Some Data
OutLength = floor(length(x) / 4) ## Desired Output Length
#### Now call to .C()
OutPut =.C("DemoAutoCor", OutVec=as.double( vector("numeric", OutLength) ),
           OutLength = as.integer(OutLength),
           InputVector = as.double(UseData),
           InputLength = as.integer(length(UseData))
);
### Gives the full outputted list() object:
OutPut

