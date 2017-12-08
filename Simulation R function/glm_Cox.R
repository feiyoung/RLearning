setwd('D:\\Ñ§Ï°\\RÓïÑÔ´úÂë')

rm(list=ls())

# 1 Load the package Survival ---------------------------------------------


library(survival)
#library(help ='survival')
data(package ='survival')
data('ovarian')
str(ovarian) # ÂÑ³²°©
ovarian
stanford2 # Ë¹Ì¹¸£ĞÄÔàÒÆÖ²Êı¾İ
#library() # see the list of available packages
library(survival) # load it. You can also
# click the pull-down manual for packages and load it.
#library(help=survival) # see the list of available functions and data sets.
data(aml) # load the data set aml
aml # see the data
stime <- c(2.2, 3, 8.4, 7.5)
status <- c(1,0,1,0)
Surv( stime, status ) 
Surv(aml$time, aml$status)
# simulate data
lifetimes <- rexp(25, rate = 0.2)
censtimes <- 5 + 5*runif(25)
ztimes <- pmin(lifetimes, censtimes)
status <- as.numeric(censtimes > lifetimes)
mydata <- data.frame(time=ztimes, status=status, x=x)

# 2 The Kaplan-Meier and Nelson-Aalen estimator ---------------------------
summary(survfit(Surv(aml$time,aml$status)~ aml$x))
fit1 <- survfit(Surv(aml$time[1:23],aml$status[1:23])~ aml$x[1:23])
class(fit1)
str(fit1)
plot(fit1)

# 3 The Log-rank test and relatives ---------------------------------------
head(aml)
survdiff(Surv(time, status)~x, data=aml)

# 4 Parametric regression models ------------------------------------------

survreg(Surv(aml$time, aml$status)~aml$x, dist="exponential")
survreg( Surv(time, status)~x, data=aml, dist="exponential")
fit1 <- survreg( Surv(time, status)~x, data=aml, dist="exponential")
predict(fit1, type="quantile", p=c(0.1, 0.5, 0.9) )

# 5 Cox regression models. ------------------------------------------------

#5.1 Simple Cox regression model. I
coxfit1 <- coxph(Surv(time, status)~x, data=aml, ties='efron')
summary(coxfit1)
basehaz(coxph(Surv(time, status)~x, data=aml))
coxfit1 <- coxph(Surv(time, status)~x, data=aml)
survfit(coxfit1, newdata=data.frame(x=1))
#5.2 Cox regression model with time change covariates. II

# 6 Simulation: Model comparison. -----------------------------------------
coxph(Surv(futime, fustat)~age+resid.ds+rx+ecog.ps, data=ovarian, ties='efron')
coxph(Surv(time, status)~age+t5, data=stanford2, ties='efron')
str(ovarian)
