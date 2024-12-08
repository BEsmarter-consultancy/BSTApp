########################## Simulation exercise: ARMA(2,1) model ##########################
rm(list = ls())
set.seed(010101)
T <- 1000
phis <- c(0.5, 0.3); thetas <- c(0.6); sig <- 0.1
y <- arima.sim(n = T, list(ar = phis, ma = thetas), sd = sig)
write.csv(y, file = "62SimARMAmodels.csv")
# p <- 2; q <- 1; f <- 1; mu0 <- 0; varmu0 <- 1; ar0 <- 0; varar0 <- 1; ma0 <- 0; varma0 <- 1;
# a0 <- 0.001; d0 <- 0.001; mcmc <- 1000; burnin <- 1000; thin <- 2
# source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/ARMA.R")
# Res <- ARMA(y = y, f = f, p = p, q = q, mu0 = mu0, varmu0 = varmu0, ar0 = ar0, varar0 = varar0,
#             ma0 = ma0, varma0 = varma0, a0 = a0, d0 = d0, mcmc = mcmc, burnin = burnin, thin = thin)
