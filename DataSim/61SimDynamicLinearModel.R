# Simulation
rm(list = ls())
set.seed(010101)
T <- 200; sig2 <- 0.5^2
x <- matrix(rnorm(T*2, mean = 1, sd = 1), T, 2)
X <- cbind(1, x); B0 <- c(1, 0.5, -0-7)
K <- length(B0)
e <- rnorm(T, mean = 0, sd = sig2^0.5)
Omega <- diag(c(0.2, 0.1, 0.3))
w <- MASS::mvrnorm(T, c(0, 0, 0), Omega)
Bt <- matrix(NA, T, K); Bt[1,] <- B0
yt <- rep(NA, T)
yt[1] <- X[1,]%*%B0 + e[1]
for(t in 1:T){
  if(t == 1){
    Bt[t,] <- w[t,]
  }else{
    Bt[t,] <- Bt[t-1,] + w[t,]
  }
  yt[t] <- X[t,]%*%Bt[t,] + e[t]
}
dfDLM <- as.data.frame(cbind(yt, x))
names(dfDLM) <- c("y", "x1", "x2")
write.csv(dfDLM, file = "61SimDynamicLinearModel.csv")
# RegLS <- lm(yt ~ x)
# SumRegLS <- summary(RegLS)
# Bp <- matrix(RegLS$coefficients, T, K, byrow = TRUE)
# S <- round(T*0.2, 0)
# for(t in S:T){
#   RegLSt <- lm(yt[1:t] ~ x[1:t,])
#   Bp[t,] <- RegLSt$coefficients
# }
# VarBp <- var(Bp)
# a.y <- (SumRegLS$sigma^2)^(-1)
# b.y <- 10*a.y
# a.theta <- (max(diag(VarBp)))^(-1)
# b.theta <- 10*a.theta
# MCMC <- 2000; burnin <- 1000; thin <- 5
# m0 = RegLS$coefficients; C0 = VarBp
# source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/DLM.R")
# Res <- DLM(y = yt, x = x, a.y = a.y, b.y = b.y, a.theta = a.theta, b.theta = b.theta, MCMC = MCMC, thin = thin, burnin = burnin)
