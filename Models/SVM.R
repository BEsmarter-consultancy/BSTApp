# Function to perform estimation of stochastic volatility models
library(stochvol)
SVM <- function(y, X, MCMC = 10000, burnin = 1000, thin = 1, mu0 = 0, sdmu0 = 100, sigma0 = 1, a0 = 5, d0 = 1.5, b0 = 0, B0 = 10000){
  # y: Dependent variable
  # X: Regressors
  # MCMC: Number of MCMC draws
  # burnin: Number of burnin
  # thin: Thining parameter
  # mu0: Prior mean mu
  # sdmu0: Prior standard deviation mu
  # sigma0: Scaling parameter of  transformed variance
  # a0: Shape parameter of prior of the transformed parameter phi
  # d0: Scale parameter of prior of the transformed parameter phi
  # b0: Prior mean coefficients of regressors
  # B0: Prior standard deviation coefficients of regressors
  # y <- y; X <- X; MCMC <- 10000; burnin <- 10000; thin <- 5
  # mu0 <- 0; sdmu0 <- 100; sigma0 <- 1; a0 <- 5; d0 <- 1.5; b0 <- 0; B0 <- 1000
  model <- stochvol::svsample(y, designmatrix = X, draws = MCMC, burnin = burnin, thin = thin,
                              priormu = c(mu0, sdmu0), priorsigma = c(sigma0), priorphi = c(a0, d0),
                              priorbeta =  c(b0, B0))
  PostSVpar <- model[["para"]][[1]][,-c(4,5)] 
  PostBetas <- model[["beta"]] 
  PostState <- model[["latent"]][[1]] 
  Summary <- list()
  Summary[[1]] <- summary(PostSVpar)
  Summary[[2]] <-summary(PostBetas)
  TestsSVpar <- list()
  TestsSVpar[[1]]<- coda::geweke.diag(PostSVpar)
  TestsSVpar[[2]] <- coda::raftery.diag(PostSVpar)
  TestsSVpar[[3]] <- coda::heidel.diag(PostSVpar)
  TestsBetas <- list()
  TestsBetas[[1]]<- coda::geweke.diag(PostBetas)
  TestsBetas[[2]] <- coda::raftery.diag(PostBetas)
  TestsBetas[[3]] <- coda::heidel.diag(PostBetas)
  StatesMean <- apply(PostState, 2, mean)
  StatesLimInf <- apply(PostState, 2, function(x){quantile(x, c(0.025))})
  StatesLimSup <- apply(PostState, 2, function(x){quantile(x, c(0.975))})
  plot_filtering_estimates <- function(df) {
    p <- ggplot(data = df, aes(x = t)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1,
                  fill = "lightblue") +
      geom_line(aes(y = mean), colour = "blue", linewidth = 0.5) +
      ylab(TeX("$B_{t}$")) + xlab("Time")
    print(p)
  }
  df <- tibble(t = seq(1, length(y)),
               mean = StatesMean,
               lower = StatesLimInf,
               upper = StatesLimSup)
  ggplot2::theme_set(theme_bw())
  PlotSV <- plot_filtering_estimates(df)

  return(list(PostSVpar = PostSVpar, PostBetas = PostBetas, MeanStates = StatesMean,
              LimInferiorStates = StatesLimInf, LimSuperiorStates = StatesLimSup,
              PlotSV = PlotSV, Summary = Summary, TestsSVpar = TestsSVpar,
              TestsBetas = TestsBetas))
  # PostSVpar: Posterior chain SV parameters
  # PostBetas: Posterior chain Betas
  # MeanStates: Mean states
  # LimInferiorStates: Inferior limit states
  # LimSuperiorStates: Superior limit states
  # PlotSV: Plot SV
  # Summary: Summary posterior chains fixed parameters
  # TestsSVpar: Diagnostics of SV parameters
  # TestsBetas: Diagnostics of Betas
}
