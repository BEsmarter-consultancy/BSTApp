# Function to perform estimation of the dynamic linear model with random walk states e independent priors of scale parameters
AuxDLMprior <- function(y, x){
  # State space model
  # y: dependent variable
  # x: regressors
  x <- as.matrix(x)
  RegLS <- lm(y ~ x)
  SumRegLS <- summary(RegLS)
  K <- dim(x)[2] + 1
  T <- length(y)
  Bp <- matrix(RegLS$coefficients, T, K, byrow = TRUE)
  S <- round(T*0.2, 0)
  for(t in S:T){
    RegLSt <- lm(y[1:t] ~ x[1:t,])
    Bp[t,] <- RegLSt$coefficients
  }
  VarBp <- var(Bp)
  a.y <- (SumRegLS$sigma^2)^(-1)
  b.y <- 10*a.y
  a.theta <- (max(diag(VarBp)))^(-1)
  b.theta <- 10*a.theta
  return(c(a.y, b.y, a.theta, b.theta))
  # a.y: prior mean of observation precision
  # b.y: prior variance of observation precision
  # a.theta: prior mean of states precision
  # b.theta: prior variance of states precision
}

DLM <- function(y, x, a.y, b.y, a.theta, b.theta, MCMC = 10000, thin = 1, burnin = 1000){
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
  plot_filtering_estimates <- function(df, i) {
    p <- ggplot(data = df, aes(x = t)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1, fill = "lightblue") +
      geom_line(aes(y = mean), colour = "blue", linewidth = 0.5) +
      ylab(TeX("$B_{t}$")) +
      xlab("Time") +
      ggtitle(paste0("State B_", i)) +   # <-- add title dynamically
      theme_bw()

    return(p)
  }

  PlotsStates <- list()
  for (i in 1:(K - 1)) {
    df <- tibble(
      t = seq(1, T),
      mean = StatesMean[-1, i],
      lower = StatesLimInf[-1, i],
      upper = StatesLimSup[-1, i]
    )
    PlotsStates[[i]] <- plot_filtering_estimates(df, i)
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

