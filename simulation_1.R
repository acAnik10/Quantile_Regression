install.packages('ExtDist')
library(quantreg)
library(ExtDist)

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


#=====================================
#EXAMPLE 1 : X~U(0,1), u~N(0,sigma^2)
#=====================================

fnorm=function(n,sigma,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  
  u=0
  Estimate=matrix(0,R,1+length(b))
  
  Actual=c(a+qnorm(p,0,sigma),b)
  
  for(i in 1:R){
    u=rnorm(n,0,sigma)
    y[i,]=a+x%*%b+u
    model=rq(y[i,]~x,p)
    Estimate[i,]=model$coefficients
  }
  FE=colMeans(Estimate)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE,MSE=MSE)
  return(Output)
}

fnorm(N,Sigma,A,B,R,p1)
fnorm(N,Sigma,A,B,R,p2)
fnorm(N,Sigma,A,B,R,p3 )

#====================================================

#=====================================
#EXAMPLE 2 : X~U(0,1), u~DE(0,sigma)
#=====================================

fDE=function(n,sigma,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  
  u=0
  Estimate=matrix(0,R,1+length(b))
  
  Actual=c(a+qLaplace(p,0,sigma),b)
  
  for(i in 1:R){
    u=rLaplace(n,0,sigma)
    y[i,]=a+x%*%b+u
    model=rq(y[i,]~x,p)
    Estimate[i,]=model$coefficients
  }
  FE=colMeans(Estimate)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE,MSE=MSE)
  return(Output)
}

fDE(N,Sigma,A,B,R,p1)
fDE(N,Sigma,A,B,R,p2)
fDE(N,Sigma,A,B,R,p3 )

#====================================================

#=======================================
#EXAMPLE 3 : X~U(0,1), u~Cauchy(0,sigma)
#=======================================

fcauchy=function(n,sigma,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  
  u=0
  Estimate=matrix(0,R,1+length(b))
  
  Actual=c(a+qcauchy(p,0,sigma),b)
  
  for(i in 1:R){
    u=rcauchy(n,0,sigma)
    y[i,]=a+x%*%b+u
    model=rq(y[i,]~x,p)
    Estimate[i,]=model$coefficients
  }
  FE=colMeans(Estimate)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance`=sigma^2,
              `Quantile`=p,
              Actual=Actual,Estimate=FE,MSE=MSE)
  return(Output)
}

fcauchy(N,Sigma,A,B,R,p1)
fcauchy(N,Sigma,A,B,R,p2)
fcauchy(N,Sigma,A,B,R,p3 )

#====================================================

#==============================
#EXAMPLE 4 : X~U(0,1), u~t(k)
#==============================

ft=function(n,k,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  
  u=0
  Estimate=matrix(0,R,1+length(b))
  
  Actual=c(a+qt(p,k),b)
  
  for(i in 1:R){
    u=rt(n,k)
    y[i,]=a+x%*%b+u
    model=rq(y[i,]~x,p)
    Estimate[i,]=model$coefficients
  }
  FE=colMeans(Estimate)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate[,i]-Actual[i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
             df=k,
              Quantile=p,
              Actual=Actual,Estimate=FE,MSE=MSE)
  return(Output)
}

ft(N,DF,A,B,R,p1)
ft(N,DF,A,B,R,p2)
ft(N,DF,A,B,R,p3 )

#==================================================