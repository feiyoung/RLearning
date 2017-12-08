
#-----------------------用于做整合数据分析
install.packages('metafuse')
library(metafuse)
?metafuse
##------------模拟实验
n <- 200    # sample size in each study
K <- 10     # number of studies
p <- 3      # number of covariates in X (including intercept)
N <- n*K    # total sample size

# the coefficient matrix, used this to set desired heterogeneous pattern (depends on p and K)
beta0 <- matrix(c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, # intercept
                  0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0, # beta_1, etc.
                  0.0,0.0,0.0,0.0,0.5,0.5,0.5,1.0,1.0,1.0), K, p)

# generate a data set, family=c("gaussian", "binomial", "poisson")
data <- datagenerator(n=n, beta0=beta0, family="gaussian", seed=123)

# prepare the input (y, X, studyID)
y       <- data$y
sid     <- data$group
X       <- data[,-c(1,ncol(data))]

# fuse slopes of X1 (it is heterogeneous with 2 groups)
metafuse(X=X, y=y, sid=sid, fuse.which=c(1), family="gaussian", intercept=TRUE, alpha=0,
         criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse slopes of X2 (it is heterogeneous with 3 groups)
metafuse(X=X, y=y, sid=sid, fuse.which=c(2), family="gaussian", intercept=TRUE, alpha=0,
         criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse all three covariates
metafuse(X=X, y=y, sid=sid, fuse.which=c(0,1,2), family="gaussian", intercept=TRUE, alpha=0,
         criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse all three covariates, with sparsity penalty
metafuse(X=X, y=y, sid=sid, fuse.which=c(0,1,2), family="gaussian", intercept=TRUE, alpha=1,
         criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)
