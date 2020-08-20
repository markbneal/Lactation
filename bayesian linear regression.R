# web example of bayesian analysis ####
# https://www.r-bloggers.com/bayesian-linear-regression-analysis-without-tears-r/

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
#basic regression
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lmfit <- lm(weight ~ group)


## function to compute the bayesian analog of the lmfit
## using non-informative priors and Monte Carlo scheme
## based on N samples
#Returns data frame
library(MASS)
bayesfit<-function(lmfit,N){
  QR<-lmfit$qr
  df.residual<-lmfit$df.residual
  R<-qr.R(QR) ## R component
  coef<-lmfit$coef
  Vb<-chol2inv(R) ## variance(unscaled)
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  
  ## now to sample residual variance
  sigma<-df.residual*s2/rchisq(N,df.residual)
  coef.sim<-sapply(sigma,function(x) mvrnorm(1,coef,Vb*x))
  ret<-data.frame(t(coef.sim))
  names(ret)<-names(lmfit$coef)
  ret$sigma<-sqrt(sigma)
  ret
}

#summarises data frame

Bayes.sum<-function(x)
{
  c("mean"=mean(x),
    "se"=sd(x),
    "t"=mean(x)/sd(x),
    "median"=median(x),
    "CrI"=quantile(x,prob=0.025),
    "CrI"=quantile(x,prob=0.975)
  )
}


set.seed(1234)  ## reproducible sim
lmfit <- lm(weight ~ group)
bf<-bayesfit(lmfit,10000)
t(apply(bf,2,Bayes.sum))

summary(lmfit)