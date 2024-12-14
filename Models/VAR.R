# Function to perform estimation of VAR models
VAR <- function(Y, p = 1, k0 = 2, k1 = 0.5, k3 = 5, H1 = 10, H2 = 4, type = "feir", cum = TRUE,
                MCMC = 10000, burnin = 1000, thin = 1){
  # Y: Variables
  # p: VAR order (integer)
  # k0: First prior coefficient Minnesota prior (positive)
  # k1: Second prior coefficient Minnesota prior (positive)
  # k3: Third prior coefficient Minnesota prior (positive)
  # MCMC: Number of MCMC iterations
  # burnin: Number of burn-in iterations
  # thin: Thinning parameter (To keep symmetry with other models, but it is not going to work)
  # H1: Impulse response horizon
  # H2: Forecast horizon
  # type: Impulse response type (forecast error or orthogonal)
  # cum: Impulse response type (accumulative or simple)
  # Y = Y; p = 1; k0 = 2; k1 = 0.5; k3 = 5; MCMC = 10000; burnin = 1000; H1 = 4; H2 = 4
  Ynew <- ts(Y)
  modelMin <- bvartools::gen_var(Ynew, p = p, deterministic = "const", iterations = MCMC, burnin = burnin)
  modelMin <- bvartools::add_priors(modelMin, minnesota = list(kappa0 = k0, kappa1 = k1, kappa3 = k3), coint_var = FALSE) # Minnesota prior
  objectMin <- bvartools::draw_posterior(modelMin) # Posterior draws
  # Impulse response
  plot_IR <- function(df, Title) {
    p <- ggplot(data = df, aes(x = t)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1, fill = "lightblue") + geom_line(aes(y = mean), colour = "blue", linewidth = 0.5) + ylab("Impulse response") + xlab("Time") + xlim(0,H1) + ggtitle(Title)
    print(p)
  }
  Nom <- colnames(Ynew)
  ImpulseResonsePlots <- list()
  ImpulseResonse <- list()
  l <- 1
  for(i in Nom){
    for(j in Nom){
      Title <- paste(i, "vs", j, sep = " ")
      irMin <- bvartools::irf.bvar(objectMin, impulse = i, response = j, n.ahead = H1, type = type, cumulative = cum) # Calculate IR
      ImpulseResonse[[l]] <- irMin
      dfNewMin <- tibble(t = 0:H1, mean = as.numeric(irMin[,2]), lower = as.numeric(irMin[,1]), upper = as.numeric(irMin[,3]))
      FigNewMin <- plot_IR(dfNewMin, Title)
      ImpulseResonsePlots[[l]] <- FigNewMin
      l <- l + 1
    }
  }
  ### Forecasting
  plot_FORE <- function(df, Title) {
    p <- ggplot(data = dfFore, aes(x = t)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1, fill = "lightblue") + geom_line(aes(y = mean), colour = "blue", linewidth = 0.5) + ylab("Forecast") + xlab("Time") + xlim(c(1,H2)) + ggtitle(Title)
    print(p)
  }
  bvar_predOR <- predict(objectMin, n.ahead = H2, new_d = rep(1, H2))
  ForecastPlots <- list()
  Forecasts <- list()
  h <- 1
  for(i in Nom){
    Title <- i
    dfFore <- tibble(t = 1:H2, mean = as.numeric(bvar_predOR[["fcst"]][[i]][,2]), lower = as.numeric(bvar_predOR[["fcst"]][[i]][,1]), upper = as.numeric(bvar_predOR[["fcst"]][[i]][,3]))
    Forecasts[[h]] <- bvar_predOR[["fcst"]][[i]]
    FigFore <- plot_FORE(dfFore, Title)
    ForecastPlots[[h]] <- FigFore
    h <- h + 1
  }
  Results <- list(ImpulseResonse = ImpulseResonse, ImpulseResonsePlots = ImpulseResonsePlots,
                  Forecasts = Forecasts, ForecastPlots = ForecastPlots)
  return(Results)
  # ImpulseResonse: Impulse response results
  # ImpulseResonsePlots: Impulse response functions
  # Forecasts: Forecasts
  # ForecastPlots : Forecast plots
}
