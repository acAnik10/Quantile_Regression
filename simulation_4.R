library(quantreg)
A=2
B=5
R=1000
Sigma=2
p1=0.05
p2=0.5
p3=0.95
N=100


#=========================================================
#EXAMPLE 5.1: X~U(0,1), Y~lognormal, eps~N(0,sigma.e^2)
#=========================================================

Sigma.e=5

flnorm1=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  
  for(i in 1:R){
    y[i,]=rlnorm(n,a+x%*%b,sigma)
    eps[i,]=rnorm(n,0,sigma.e)
    y.e[i,]=log(y[i,])+eps[i,]
    model1=rq(log(y[i,])~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate2[,i]-Estimate1[,i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE
  )
  return(Output)
}

flnorm1(N,Sigma,Sigma.e,A,B,R,p1)
flnorm1(N,Sigma,Sigma.e,A,B,R,p2)
flnorm1(N,Sigma,Sigma.e,A,B,R,p3)
#========================================================

#=========================================================
#EXAMPLE 5.2: X~U(0,1), Y~Lognormal, eps~C(0,sigma.e)
#=========================================================

Sigma.e=5

flnorm2=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  
  for(i in 1:R){
    y[i,]=rlnorm(n,a+x%*%b,sigma)
    eps[i,]=rcauchy(n,0,sigma.e)
    y.e[i,]=log(y[i,])+eps[i,]
    model1=rq(log(y[i,])~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate2[,i]-Estimate1[,i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE
  )
  return(Output)
}

flnorm2(N,Sigma,Sigma.e,A,B,R,p1)
flnorm2(N,Sigma,Sigma.e,A,B,R,p2)
flnorm2(N,Sigma,Sigma.e,A,B,R,p3)
#========================================================
#=========================================================
#EXAMPLE 5.3: X~U(0,1), Y~Gamma, eps~Lognormal(0,sigma.e^2)
#=========================================================

Sigma.e=5

flnorm3=function(n,sigma,sigma.e,a,b,R,p){
  x=matrix(runif(n*length(b)),ncol=length(b))
  
  y=matrix(0,R,n)
  eps=matrix(0,R,n)
  y.e=matrix(0,R,n)
  
  u=0
  Estimate1=matrix(0,R,1+length(b))
  Estimate2=matrix(0,R,1+length(b))
  
  for(i in 1:R){
    y[i,]=rlnorm(n,a+x%*%b,sigma)
    eps[i,]=rlnorm(n,0,sigma.e)
    y.e[i,]=log(y[i,])+eps[i,]
    model1=rq(log(y[i,])~x,p)
    model2=rq(y.e[i,]~x,p)
    Estimate1[i,]=model1$coefficients
    Estimate2[i,]=model2$coefficients
  }
  FE1=colMeans(Estimate1)
  FE2=colMeans(Estimate2)
  MSE=0
  for(i in 1:(1+length(b))){
    MSE[i]=mean((Estimate2[,i]-Estimate1[,i])^2)
  }
  # names(MSE)=c('a','b')
  # colnames(Estimate)=c('a','b')
  # names(Actual)=c('a','b')
  Output=list(`Sample Size`=n,
              `Error Variance (Error in y)`=sigma.e^2,
              `Quantile`=p,
              Estimate=FE1,
              `Estimate(Error in y)`=FE2,
              MSE=MSE
  )
  return(Output)
}

flnorm3(N,Sigma,Sigma.e,A,B,R,p1)
flnorm3(N,Sigma,Sigma.e,A,B,R,p2)
flnorm3(N,Sigma,Sigma.e,A,B,R,p3)
#========================================================
