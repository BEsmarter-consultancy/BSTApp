########################## Simulation exercise: Stochastic volatility models #########################
rm(list = ls())
set.seed(010101)
T <- 1250; K <- 2
X <- matrix(rnorm(T*K), T, K)
B <- c(0.5, 0.3); mu <- -10; phi <- 0.95; sigma <- 0.3
h <- numeric(T)
y <- numeric(T)
h[1] <- rnorm(1, mu, sigma / sqrt(1 - phi^2))  # Initial state
y[1] <- X[1,]%*%B + rnorm(1, 0, exp(h[1] / 2))           # Initial observation
for (t in 2:T) {
  h[t] <- mu + phi*(h[t-1]-mu) + rnorm(1, 0, sigma)
  y[t] <- X[t,]%*%B + rnorm(1, 0, sd = exp(0.5*h[t]))
}
df <- as.data.frame(cbind(y, X))
names(df) <- c("y", "x1", "x2")
write.csv(df, file = "63SimStochasticVolatility.csv")
# source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/SVM.R")
# y <- y; X <- X; MCMC <- 10000; burnin <- 10000; thin <- 5
# mu0 <- 0; sdmu0 <- 100; sigma0 <- 1; a0 <- 5; d0 <- 1.5; b0 <- 0; B0 <- 1000
# Res <- SVM(y = y, X = X, MCMC = MCMC, burnin = burnin, thin = thin,
#            mu0 = mu0, sdmu0 = sdmu0, sigma0 = sigma0, a0 = a0, d0 = d0, b0 = b0, B0 = B0)
