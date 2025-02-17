setwd("C:/ANDRES/Portatil/Bayesian Econometrics/UserInterface/BS-UId/Multivariate")
library(bayesm)
rm(list=ls())
set.seed(66)
beta1=c(1,2)
beta2=c(1,-1,-2)
nobs=1000
nreg=2
iota=c(rep(1,nobs))
X1=cbind(iota,runif(nobs))
X2=cbind(iota,runif(nobs),runif(nobs))
Sigma=matrix(c(.5,.2,.2,.5),ncol=2)
U=chol(Sigma)
E=matrix(rnorm(2*nobs),ncol=2)%*%U
y1=X1%*%beta1+E[,1]
y2=X2%*%beta2+E[,2]
dat<-cbind(y1,y2,X1,X2)




## simulate data from SUR
beta1 = c(1,2)
beta2 = c(1,-1,-2)
nobs = 100
nreg = 2
iota = c(rep(1, nobs))
X1 = cbind(iota, runif(nobs))
X2 = cbind(iota, runif(nobs), runif(nobs))
Sigma = matrix(c(0.5, 0.2, 0.2, 0.5), ncol=2)
U = chol(Sigma)
E = matrix(rnorm(2*nobs),ncol=2)%*%U
y1 = X1%*%beta1 + E[,1]
y2 = X2%*%beta2 + E[,2]
## run Gibbs Sampler
regdata = NULL
regdata[[1]] = list(y=y1, X=X1)
regdata[[2]] = list(y=y2, X=X2)
Prior <- list(betabar = rep(0, 5), A = diag(5), nu = 5) # There is problem setting V using rsurGibbs
out = rsurGibbs(Data=list(regdata=regdata), Mcmc=list(R=10), Prior = Prior)

SumDiagSUR(out$bet,Posteriors21()$Sig[,],input$itMV+input$burninMV,input$burninMV,as.numeric(input$keepMV))

write.csv(dat,file="22SimSUR.csv")
