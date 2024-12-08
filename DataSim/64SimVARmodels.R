########################## Vector Autoregressive models: Simulation ########################## 
rm(list = ls())
set.seed(010101)
T <- 300; M <- 3
SIGMA <- matrix(c(2.25, 0, 0, 0, 1, 0.5, 0, 0.5, 0.74), M, M )
U <- MASS::mvrnorm(T, rep(0, M), SIGMA)
A <- matrix(c(0.5, 0.1, 0, 0, 0.1, 0.2, 0, 0.3, 0.3), M, M ) 
v <- c(1.4, 1.9, 1.1)
Y <- matrix(NA, T, M)
Y[1, ] <- solve(diag(M)-A)%*%v + U[1,]
for(t in 2:T){
  Y[t, ] <- v + A%*%Y[t-1,] + U[t,]
}
df <- as.data.frame(Y)
names(df) <- c("y1", "y2", "y3")
write.csv(df, file = "64SimVARmodels.csv")
# source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/VAR.R")
# Y = Y; p = 1; k0 = 2; k1 = 0.5; k3 = 5; MCMC = 10000; burnin = 1000; thin = 1
# H1 = 4; H2 = 4; type = "oir"; cum = FALSE # type = "feir" and "oir" for forecast error and orthogonalized IRs
# Res <- VAR(Y = Y, p = p, k0 = k0, k1 = k1, k3 = k3, H1 = H1, H2 = H2, type = type, cum = cum,
#            MCMC = MCMC, burnin = burnin, thin = thin)

