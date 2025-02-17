########################## Dynamic Bayesian model average: Simulation exercise ########################## 
rm(list = ls())
set.seed(010101)
T <- 500; K <- 3
X <- matrix(rnorm(T*K, mean = 0.5, sd = 0.8), T, K)
B1 <- 0.5
B2t <- seq(1, 2, length.out=T )
a <- 0.75
B3t <- c(rep(-1,round(a*T)), rep(0,round((1-a)*T)))
B4 <- 1.2
sigma <- 1; mu <- rnorm(T, 0, sigma)
y <- B1 + X[,1]*B2t + X[,2]*B3t + X[,3]*B4 + mu
df <- as.data.frame(cbind(y, X))
names(df) <- c("y", "x1", "x2", "x3")
write.csv(df, file = "55SimDynamicBMA.csv")
combs <- expand.grid(c(0,1), c(0,1), c(0,1))
write.csv(combs, file = "55SimModels.csv")
# source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/DBMA.R")
# y = y; X = X; models = combs; lambda = 0.99; delta = 0.99
# Res <- DBMA(y = y, X = X, models = models, lambda = lambda, delta = delta)

