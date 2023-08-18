library(quantreg)

#parameter values
#================

A=2
B=5
Sigma=2
DF=2
R=1000
p1=0.05
p2=0.5
p3=0.95
N=100


#=========================================================
#EXAMPLE 1.1 : X~U(0,1), u~N(0,sigma^2), eps~N(0,sigma.e^2)
#=========================================================

Sigma.e=5

fnorm1=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  Actual=c(a+qnorm(p,0,sigma),b)
  
  for(i in 1:R){
    u=rnorm(n,0,sigma)
    y[i,]=a+x%*%b+u
    eps[i,]=rnorm(n,0,sigma.e)
    y.e[i,]=y[i,]+eps[i,]
    model1=rq(y[i,]~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE1=0
  MSE2=0
  for(i in 1:(1+length(b))){
    MSE1[i]=mean((Estimate1[,i]-Actual[i])^2)
    MSE2[i]=mean((Estimate2[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE1,
              `MSE(Error in y)`=MSE2
              )
  return(Output)
}

fnorm1(N,Sigma,Sigma.e,A,B,R,p1)
fnorm1(N,Sigma,Sigma.e,A,B,R,p2)
fnorm1(N,Sigma,Sigma.e,A,B,R,p3)
#========================================================

#=========================================================
#EXAMPLE 1.2 : X~U(0,1), u~N(0,sigma^2), eps~C(0,sigma.e)
#=========================================================

Sigma.e=5

fnorm2=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  Actual=c(a+qnorm(p,0,sigma),b)
  
  for(i in 1:R){
    u=rnorm(n,0,sigma)
    y[i,]=a+x%*%b+u
    eps[i,]=rcauchy(n,0,sigma.e)
    y.e[i,]=y[i,]+eps[i,]
    model1=rq(y[i,]~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE1=0
  MSE2=0
  for(i in 1:(1+length(b))){
    MSE1[i]=mean((Estimate1[,i]-Actual[i])^2)
    MSE2[i]=mean((Estimate2[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE1,
              `MSE(Error in y)`=MSE2
  )
  return(Output)
}

fnorm2(N,Sigma,Sigma.e,A,B,R,p1)
fnorm2(N,Sigma,Sigma.e,A,B,R,p2)
fnorm2(N,Sigma,Sigma.e,A,B,R,p3)
#=====================================================

#==================================================================
#EXAMPLE 1.3 : X~U(0,1), u~N(0,sigma^2), eps~lognormal(0,sigma.e^2)
#==================================================================

Sigma.e=5

fnorm3=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  Actual=c(a+qnorm(p,0,sigma),b)
  
  for(i in 1:R){
    u=rnorm(n,0,sigma)
    y[i,]=a+x%*%b+u
    eps[i,]=rlnorm(n,0,sigma.e)
    y.e[i,]=y[i,]+eps[i,]
    model1=rq(y[i,]~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE1=0
  MSE2=0
  for(i in 1:(1+length(b))){
    MSE1[i]=mean((Estimate1[,i]-Actual[i])^2)
    MSE2[i]=mean((Estimate2[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE1,
              `MSE(Error in y)`=MSE2
  )
  return(Output)
}

fnorm3(N,Sigma,Sigma.e,A,B,R,p1)
fnorm3(N,Sigma,Sigma.e,A,B,R,p2)
fnorm3(N,Sigma,Sigma.e,A,B,R,p3)
#======================================================

#=========================================================
#EXAMPLE 1.4 : X~U(0,1), u~N(0,sigma^2), eps~N(0,x^2)
#=========================================================

