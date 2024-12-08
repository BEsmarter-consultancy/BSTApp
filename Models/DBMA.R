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
  return(list(PMP = PMP, BetasBMAmean = BetasBMAmean, BetasBMAsd = BetasBMAsd,
              ForecastsMean = ForecastsMean))
  # PMP: Dynamic posterior model probability
  # BetasBMAmean: Posterior BMA means
  # BetasBMAsd: Posterior BMA standard deviation
  # ForecastsMean: BMA forecast
}