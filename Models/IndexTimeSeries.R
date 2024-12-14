# Index: Time series
source("C:/Users/aramir21/Desktop/GUIwebV1/BSTApp/Models/DLM.R")
Res <- DLM(y = yt, x = x, a.y = a.y, b.y = b.y, a.theta = a.theta, b.theta = b.theta, MCMC = MCMC, thin = thin, burnin = burnin)
