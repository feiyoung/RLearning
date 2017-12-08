MonteCarloPi <-
function(nsim){
  x <- 2*runif(nsim)-1
  y <- 2*runif(nsim)-1
  inx <- which((x^2+y^2)<=1)
  return ((length(inx)/nsim)*4)
}
