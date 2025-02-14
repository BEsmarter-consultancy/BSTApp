# Function to perform estimation of VAR models
library(dma)
DBMA <- function(y, X, models, lambda = 0.99, delta = 0.99){
  # y: Dependent variable
  # X: Regressors
  # models: Matrix of models, the variables are in the columns,
  #         and the rows are model indicators (0/1)
  # lambda: Forgetting parameter of covariance matrix
  # delta: Forgetting parameter transition model probabilities
  # y = y; X = X; models = combs; lambda = 0.99; gamma = 0.99
  T0 <- 0
  dma.test <- dma::dma(X, y, models.which = models, lambda = lambda, gamma = delta, initialperiod = T0)
  PMP <- dma.test[["pmp"]]
  BetasBMAmean <- dma.test[["thetahat.ma"]]
  BetasBMAsd <- dma.test[["Vtheta.ma"]]^0.5
  ForecastsMean <- dma.test[["yhat.ma"]]
  Plot <- function(PMP){
    # Generate default labels if column names are not provided
    if (is.null(colnames(PMP))) {
      colnames(PMP) <- paste("Model", seq_len(ncol(PMP)))
    }
    matplot(PMP, type = "l", main = "Posterior model probabilities", xlab = "Time", ylab = "PMP", col = rainbow(ncol(PMP)))
    if(dim(PMP)[2] <= 10){
      # Add a legend
      legend(
        x = "topleft",
        legend = colnames(PMP),           # Use column names as labels
        col = rainbow(ncol(PMP)),         # Match colors used in the plot
        pch = 1:ncol(PMP),                # Match point styles
        lty = 1:ncol(PMP)                 # Line types for each series
      )
    }
    plot <- recordPlot()
    return(plot)
  }
  PlotPMP <- Plot(PMP)
  return(list(PMP = PMP, BetasBMAmean = BetasBMAmean, BetasBMAsd = BetasBMAsd,
              ForecastsMean = ForecastsMean, PlotPMP = PlotPMP))
  # PMP: Dynamic posterior model probability
  # BetasBMAmean: Posterior BMA means
  # BetasBMAsd: Posterior BMA standard deviation
  # ForecastsMean: BMA forecast
  # PlotPMP: Plot of the posterior model probabilities in time
}
