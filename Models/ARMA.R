# Funtion to perform estimation of ARMA models
library(bayesforecast)
ARMA <- function(y, f, p, q, mu0, varmu0, ar0, varar0, ma0, varma0, a0, d0, mcmc, burnin, thin){
  # y: time series
  # f: frequency (times per year)
  # p: AR order
  # q: MA order
  # mu0: prior mean intercept
  # varmu0: prior variance intercept
  # ar0: prior mean ARs
  # varar0: prior variance ARs
  # ma0: prior mean MAs
  # varma0: prior variance MAs
  # a0/2: shape parameter variance of the model
  # d0/2: rate parameter variance of the model
  # mcmc: MCMC iterations
  # burnin: burnin iterations
  # thin: thin paramater
  # p <- 2; q <- 1; f <- 1; mu0 <- 0; varmu0 <- 1; ar0 <- 0; varar0 <- 1; ma0 <- 0; varma0 <- 1; 
  # a0 <- 0.001; d0 <- 0.001; mcmc <- 1000; burnin <- 1000; thin <- 2 
  yt <- ts(y, frequency = f)
  tot <- mcmc+burnin
  sf1 <- bayesforecast::stan_sarima(yt, order = c(p, 0, q), prior_mu0 = normal(mu0, varmu0^0.5), 
                                    prior_ar = normal(ar0, varar0^0.5), prior_ma = normal(ma0, varma0^0.5), prior_sigma0 = inverse.gamma(a0/2, d0/2),
                                    seasonal = c(0, 0, 0), iter = tot, warmup = burnin, chains = 1)
  keep <- seq(burnin+1, tot, thin)
  Postmu <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][["mu0"]][keep])
  Postsig <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][["sigma0"]][keep])
  summary(Postmu)
  summary(Postsig)
  if(p != 0){
    ARcoef <- list()
    for(i in 1:p){
      ari <- paste("ar0","[",i,"]", sep = "")
      ARcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[ari]][keep]) 
      print(summary(ARcoef[[i]]))
    }
  }
  if(q != 0){
    MAcoef <- list()
    for(i in 1:q){
      mai <- paste("ma0","[",i,"]", sep = "")
      MAcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[mai]][keep])
      print(summary(MAcoef[[i]]))
    }
  }
  return(list(mu = Postmu, sigma = Postsig, AR = ARcoef, MA = MAcoef))
}

