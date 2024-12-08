# Function to perform estimation of ARMA models
library(bayesforecast)
ARMA <- function(y, f = 1, p = 1, q = 1, mu0 = 0, varmu0 = 1, ar0 = 0, varar0 = 0.5^2, ma0 = 0, varma0 = 0.5^2, a0 = 0.01, d0 = 0.01, mcmc = 1000, burnin = 1000, thin = 1){
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
  # burnin: burn-in iterations
  # thin: thin parameter
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
  Summary <- list()
  Summary[[1]] <- summary(Postmu)
  Summary[[2]] <- summary(Postsig)
  TestsVarMu <- list()
  TestsVarMu[[1]]<- coda::geweke.diag(Postmu)
  TestsVarMu[[2]] <- coda::raftery.diag(Postmu)
  TestsVarMu[[3]] <- coda::heidel.diag(Postmu)
  TestsVarSigma <- list()
  TestsVarSigma[[1]]<- coda::geweke.diag(Postsig)
  TestsVarSigma[[2]] <- coda::raftery.diag(Postsig)
  TestsVarSigma[[3]] <- coda::heidel.diag(Postsig)
  if(p != 0){
    SummaryAR <- list()
    ARcoef <- list()
    ARTestgeweke <- list()
    ARTestraftery <- list()
    ARTestheidel <- list()
    for(i in 1:p){
      ari <- paste("ar0","[",i,"]", sep = "")
      ARcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[ari]][keep])
      SummaryAR[[i]] <- summary(ARcoef[[i]])
      ARTestgeweke[[i]] <- coda::geweke.diag(ARcoef[[i]])
      ARTestraftery[[i]] <- coda::raftery.diag(ARcoef[[i]])
      ARTestheidel[[i]] <- coda::heidel.diag(ARcoef[[i]])
    }
  }
  if(q != 0){
    MAcoef <- list()
    SummaryMA <- list()
    MATestgeweke <- list()
    MATestraftery <- list()
    MATestheidel <- list()
    for(i in 1:q){
      mai <- paste("ma0","[",i,"]", sep = "")
      MAcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[mai]][keep])
      SummaryMA[[i]] <- summary(MAcoef[[i]])
      MATestgeweke[[i]] <- coda::geweke.diag(MAcoef[[i]])
      MATestraftery[[i]] <- coda::raftery.diag(MAcoef[[i]])
      MATestheidel[[i]] <- coda::heidel.diag(MAcoef[[i]])
    }
  }
  if(p != 0 & q != 0){
    Results <- list(mu = Postmu, sigma = Postsig, AR = ARcoef, MA = MAcoef,
                    SummaryAR = SummaryAR, ARTestgeweke = ARTestgeweke,
                    ARTestraftery = ARTestraftery, ARTestheidel = ARTestheidel,
                    SummaryMA = SummaryMA, MATestgeweke = MATestgeweke,
                    MATestraftery = MATestraftery, MATestheidel = MATestheidel,
                    Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma)
  }else{
    if(p != 0 & q == 0){
      Results <- list(mu = Postmu, sigma = Postsig, AR = ARcoef,
                      SummaryAR = SummaryAR, ARTestgeweke = ARTestgeweke,
                      ARTestraftery = ARTestraftery, ARTestheidel = ARTestheidel,
                      Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma)
    }else{
      if(p == 0 & q != 0){
        Results <- list(mu = Postmu, sigma = Postsig, MA = MAcoef,
                        SummaryMA = SummaryMA, MATestgeweke = MATestgeweke,
                        MATestraftery = MATestraftery, MATestheidel = MATestheidel,
                        Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma)
      }else{
        Results <- list(mu = Postmu, sigma = Postsig,
                        Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma)
      }
    }
  }

  return(Results)
  # mu: Posterior chain intercept
  # sigma: Posterior chain sigma
  # AR: Posterior chain AR coefs
  # MA: Posterior chain MA coefs
  # SummaryAR: Summary AR coefs
  # ARTestgeweke: Geweke test
  # ARTestraftery: Raftery test
  # ARTestheidel: Heidel test
  # SummaryMA: Summary MA coefs
  # MATestgeweke: Geweke test
  # MATestraftery: Raftery test
  # MATestheidel: Heidel test
  # Summary: Summary intercept y sigma
  # TestsVarMu: Geweke, Raftery and Heidel tests intercept
  # TestsVarSigma: Geweke, Raftery and Heidel tests sigma
}

