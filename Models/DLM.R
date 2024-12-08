# Function to perform estimation of the dynamic linear model with random walk states e independent priors of scale parameters
library(dplyr)
library(ggplot2)
library(latex2exp)
library(dlm)
DLM <- function(y, x, a.y, b.y, a.theta, b.theta, MCMC, thin, burnin){
  # State space model
  # y: dependent variable
  # x: regressors
  # a.y: prior mean of observation precision
  # b.y: prior variance of observation precision
  # a.theta: prior mean of states precision
  # b.theta: prior variance of states precision
  # MCMC: MCMC iterations
  # thin: thin parameter
  # burnin: burn-in parameter
  # y = yt; x = x; a.y = a.y; b.y = b.y; a.theta = a.theta; b.theta = b.theta; MCMC = 1000; thin = 1; burnin = 100
  T <- length(y)
  if(is.null(dim(x)[2])){
    K <- 3
  }else{
    K <- dim(x)[2] + 2
  }
  ModelReg <- function(par){
    Mod <- dlm::dlmModReg(x, dV = exp(par[1]), dW = exp(par[2:K]))
    return(Mod)
  }
  gibbsOut <- dlm::dlmGibbsDIG(y, mod = dlm::dlmModReg(x), a.y = a.y, b.y = b.y, a.theta = a.theta, b.theta = b.theta, n.sample = MCMC, thin = thin, save.states = TRUE)
  VarObs <- coda::mcmc(gibbsOut[["dV"]])
  VarStates <- coda::mcmc(gibbsOut[["dW"]])
  Summary <- list()
  Summary[[1]] <- summary(VarObs)
  Summary[[2]] <-summary(VarStates)
  TestsVarObs <- list()
  TestsVarObs[[1]]<- coda::geweke.diag(VarObs)
  TestsVarObs[[2]] <- coda::raftery.diag(VarObs)
  TestsVarObs[[3]] <- coda::heidel.diag(VarObs)
  TestsVarStates <- list()
  TestsVarStates[[1]]<- coda::geweke.diag(VarStates)
  TestsVarStates[[2]] <- coda::raftery.diag(VarStates)
  TestsVarStates[[3]] <- coda::heidel.diag(VarStates)
  StatesMean <- apply(gibbsOut[["theta"]], c(1,2), mean)
  StatesLimInf <- apply(gibbsOut[["theta"]], c(1,2), function(x){quantile(x, c(0.025))})
  StatesLimSup <- apply(gibbsOut[["theta"]], c(1,2), function(x){quantile(x, c(0.975))})
  plot_filtering_estimates <- function(df) {
    p <- ggplot(data = df, aes(x = t)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1,
                  fill = "lightblue") +
      geom_line(aes(y = mean), colour = "blue", linewidth = 0.5) +
      ylab(TeX("$B_{t}$")) + xlab("Time")
    print(p)
  }
  PlotsStates <- list()
  for(i in 1:(K-1)){
    df <- tibble(t = seq(1, T),
                 mean = StatesMean[-1, i],
                 lower = StatesLimInf[-1, i],
                 upper = StatesLimSup[-1, i])
    ggplot2::theme_set(theme_bw())
    PlotsStates[[i]] <- plot_filtering_estimates(df)
  }
  return(list(VarianceObs = VarObs, VarianceStates = VarStates, MeanStates = StatesMean,
              LimInferiorStates = StatesLimInf, LimSuperiorStates = StatesLimSup,
              PlotStates = PlotsStates, Summary = Summary, TestsVarObs = TestsVarObs,
              TestsVarStates = TestsVarStates))
  # VarianceObs: Posterior chain variance observable equation
  # VarianceStates: Posterior chain variances states
  # MeanStates: Mean states
  # LimInferiorStates: Inferior limit states
  # LimSuperiorStates: Superior limit states
  # PlotsStates: Plot of states
  # Summary: Summary posterior chains variances
  # TestsVarObs: Diagnostics variance of observation equation
  # TestsVarStates: Diagnostics variance of state equations
}

