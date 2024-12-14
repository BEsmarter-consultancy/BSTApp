# Function to perform estimation of ARMA models
ARMA <- function(y, f = 1, p = 1, q = 1, mu0 = 0, varmu0 = 1, ar0 = 0, varar0 = 0.5^2, ma0 = 0, varma0 = 0.5^2, a0 = 0.01, d0 = 0.01, mcmc = 1000, burnin = 1000, thin = 1){
  # y: time series
  # f: frequency (times per year: 1 (annual data), 4 (quarterly data), 12 (monthly data))
  # p: AR order (integer)
  # q: MA order (integer)
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

  # Plots
  Plot<- function(betadraw){
    hist(betadraw, breaks=20,freq=FALSE, xlab="Parameter", main="", col="lightgreen")
    lines(density(betadraw,na.rm = TRUE), col="red", lwd=2)
    abline(h = NULL, v = c(quantile(betadraw,c(0.025, 0.975))), col = "purple", lwd=2)
    text(quantile(betadraw,c(0.025)),y=1, "Quantile 2.5%", col = "black", adj = c(0,-0.5), cex=0.75)
    text(quantile(betadraw,c(0.975)),y=1, "Quantile 97.5%", col = "black", adj = c(0,-0.5), cex=0.75)
    abline(h = NULL, v = c(quantile(betadraw,c(0.5))), col = "red", lwd=3)
    abline(h = NULL, v = mean(betadraw), col = "blue", lwd=2)
    legend("topleft",inset=.05,cex = 0.75,c("Median","Mean"),horiz=TRUE,lty=c(1,1),lwd=c(1,1),col=c("red","blue"),bg="grey96")
    plot <- recordPlot()
    return(plot)
  }

  Plot.trace<- function(betadraw){
    traceplot(mcmc(betadraw), main = "Trace Plot", xlab="Iteration", ylab= "Parameter", col= "blue")
    plot <- recordPlot()
    return(plot)}
  Plot.corr<- function(betadraw){
    autocorr.plot(mcmc(betadraw), main = "Autocorrelation", col="blue")
    plot <- recordPlot()
    return(plot)}

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
  PlotMu <- Plot(Postmu)
  PlotSig <- Plot(Postsig)
  TestsVarMu <- list()
  TestsVarMu[[1]]<- coda::geweke.diag(Postmu)
  TestsVarMu[[2]] <- coda::raftery.diag(Postmu)
  TestsVarMu[[3]] <- coda::heidel.diag(Postmu)
  TestsVarSigma <- list()
  TestsVarSigma[[1]]<- coda::geweke.diag(Postsig)
  TestsVarSigma[[2]] <- coda::raftery.diag(Postsig)
  TestsVarSigma[[3]] <- coda::heidel.diag(Postsig)
  PlotTraceMu <- Plot.trace(Postmu)
  PlotTraceSig <- Plot.trace(Postsig)
  PlotCorrMu <- Plot.corr(Postmu)
  PlotCorrSig <- Plot.corr(Postsig)
  if(p != 0){
    SummaryAR <- list()
    ARcoef <- list()
    ARTestgeweke <- list()
    ARTestraftery <- list()
    ARTestheidel <- list()
    PlotsAR <- list()
    PlotsARtrace <- list()
    PlotsARcorr <- list()
    for(i in 1:p){
      ari <- paste("ar0","[",i,"]", sep = "")
      ARcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[ari]][keep])
      SummaryAR[[i]] <- summary(ARcoef[[i]])
      ARTestgeweke[[i]] <- coda::geweke.diag(ARcoef[[i]])
      ARTestraftery[[i]] <- coda::raftery.diag(ARcoef[[i]])
      ARTestheidel[[i]] <- coda::heidel.diag(ARcoef[[i]])
      PlotsAR[[i]] <- Plot(ARcoef[[i]])
      PlotsARtrace[[i]] <- Plot.trace(ARcoef[[i]])
      PlotsARcorr[[i]] <- Plot.corr(ARcoef[[i]])
    }
  }
  if(q != 0){
    MAcoef <- list()
    SummaryMA <- list()
    MATestgeweke <- list()
    MATestraftery <- list()
    MATestheidel <- list()
    PlotsMA <- list()
    PlotsMAtrace <- list()
    PlotsMAcorr <- list()
    for(i in 1:q){
      mai <- paste("ma0","[",i,"]", sep = "")
      MAcoef[[i]] <- coda::mcmc(sf1[["stanfit"]]@sim[["samples"]][[1]][[mai]][keep])
      SummaryMA[[i]] <- summary(MAcoef[[i]])
      MATestgeweke[[i]] <- coda::geweke.diag(MAcoef[[i]])
      MATestraftery[[i]] <- coda::raftery.diag(MAcoef[[i]])
      MATestheidel[[i]] <- coda::heidel.diag(MAcoef[[i]])
      PlotsMA[[i]] <- Plot(MAcoef[[i]])
      PlotsMAtrace[[i]] <- Plot.trace(MAcoef[[i]])
      PlotsMAcorr[[i]] <- Plot.corr(MAcoef[[i]])
    }
  }
  if(p != 0 & q != 0){
    Results <- list(mu = Postmu, sigma = Postsig, AR = ARcoef, MA = MAcoef,
                    SummaryAR = SummaryAR, ARTestgeweke = ARTestgeweke,
                    ARTestraftery = ARTestraftery, ARTestheidel = ARTestheidel,
                    SummaryMA = SummaryMA, MATestgeweke = MATestgeweke,
                    MATestraftery = MATestraftery, MATestheidel = MATestheidel,
                    Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma,
                    PlotMu = PlotMu, PlotsMutrace = PlotTraceMu, PlotMucorr = PlotCorrMu,
                    PlotSigma = PlotSig, PlotSigmatrace = PlotTraceSig, PlotSigmacorr = PlotCorrSig,
                    PlotsAR = PlotsAR, PlotsARtrace = PlotsARtrace, PlotsARcorr = PlotsARcorr,
                    PlotsMA = PlotsMA, PlotsMAtrace = PlotsMAtrace, PlotsMAcorr = PlotsMAcorr)
  }else{
    if(p != 0 & q == 0){
      Results <- list(mu = Postmu, sigma = Postsig, AR = ARcoef,
                      SummaryAR = SummaryAR, ARTestgeweke = ARTestgeweke,
                      ARTestraftery = ARTestraftery, ARTestheidel = ARTestheidel,
                      Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma,
                      PlotMu = PlotMu, PlotsMutrace = PlotTraceMu, PlotMucorr = PlotCorrMu,
                      PlotSigma = PlotSig, PlotSigmatrace = PlotTraceSig, PlotSigmacorr = PlotCorrSig,
                      PlotsAR = PlotsAR, PlotsARtrace = PlotsARtrace, PlotsARcorr = PlotsARcorr)
    }else{
      if(p == 0 & q != 0){
        Results <- list(mu = Postmu, sigma = Postsig, MA = MAcoef,
                        SummaryMA = SummaryMA, MATestgeweke = MATestgeweke,
                        MATestraftery = MATestraftery, MATestheidel = MATestheidel,
                        Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma,
                        PlotMu = PlotMu, PlotsMutrace = PlotTraceMu, PlotMucorr = PlotCorrMu,
                        PlotSigma = PlotSig, PlotSigmatrace = PlotTraceSig, PlotSigmacorr = PlotCorrSig,
                        PlotsMA = PlotsMA, PlotsMAtrace = PlotsMAtrace, PlotsMAcorr = PlotsMAcorr)
      }else{
        Results <- list(mu = Postmu, sigma = Postsig,
                        Summary = Summary, TestsVarMu = TestsVarMu, TestsVarSigma = TestsVarSigma,
                        PlotMu = PlotMu, PlotsMutrace = PlotTraceMu, PlotMucorr = PlotCorrMu,
                        PlotSigma = PlotSig, PlotSigmatrace = PlotTraceSig, PlotSigmacorr = PlotCorrSig)
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
  # Plotsmu: Plot mu posterior density
  # Plotsmutrace: Plot mu posterior traces
  # Plotsmucorr: Plot mu posterior autocorrelation
  # PlotsSigma: Plot sigma posterior density
  # PlotsSigmatrace: Plot sigma posterior traces
  # PlotsSigmacorr: Plots sigma posterior autocorrelation
  # PlotsAR: Plots AR posterior density
  # PlotsARtrace: Plots AR posterior traces
  # PlotsARcorr: Plots AR posterior autocorrelation
  # PlotsMA: Plots MA posterior density
  # PlotsMAtrace: Plots MA posterior traces
  # PlotsMAcorr: Plots MA posterior autocorrelation
}

