# Index: Time series
rm(list = ls())
library(dplyr)
library(ggplot2)
library(latex2exp)
library(dlm)
library(bayesforecast)
library(stochvol)
library(bvartools)

##### Dynamic Linear Models ########
dataDLM <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/61SimDynamicLinearModel.csv")
source("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/Models/DLM.R")
DefaultPrior <- AuxDLMprior(y = dataDLM[,1], x = dataDLM[,-1])
# Input: Default hyperparameters of dynamic linear model to be used in function DLM
# State space model
# y: dependent variable
# x: regressors

# Output: Default hyperparameters of dynamic linear model to be used in function DLM
# a.y: prior mean of observation precision
# b.y: prior variance of observation precision
# a.theta: prior mean of states precision
# b.theta: prior variance of states precision

a.y <- DefaultPrior[1]; b.y <- DefaultPrior[2]; a.theta <- DefaultPrior[3]; b.theta <- DefaultPrior[4]
ResDLM <- DLM(y = dataDLM[,1], x = dataDLM[,-1], a.y = a.y, b.y = b.y, a.theta = a.theta, b.theta = b.theta, MCMC = 10000, thin = 1, burnin = 1000)
# Input: Results dynamic linear model
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

# Output: Results dynamic linear model
# VarianceObs: Posterior chain variance observable equation --> To download
# VarianceStates: Posterior chain variances states --> To download
# MeanStates: Mean states --> To download
# LimInferiorStates: Inferior limit states --> To download
# LimSuperiorStates: Superior limit states --> To download
# PlotsStates: Plot of states --> To display in GUI
# Summary: Summary posterior chains variances --> To display in GUI
# TestsVarObs: Diagnostics variance of observation equation --> To display in GUI
# TestsVarStates: Diagnostics variance of state equations --> To display in GUI

##### ARMA Models ########
dataARMA <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/62SimARMAmodels.csv")
source("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/Models/ARMA.R")
ResARMA <- ARMA(y = dataARMA[,1], f = 1, p = 1, q = 1, mu0 = 0, varmu0 = 1, ar0 = 0, varar0 = 0.5^2, ma0 = 0, varma0 = 0.5^2, a0 = 0.01, d0 = 0.01, mcmc = 1000, burnin = 1000, thin = 1)
# Input: ARMA models
# y: time series
# f: frequency (times per year: 1 (annual data), 4 (quarterly data), 12 (monthly data))
# p: AR order (integer)
# q: MA order (integer)
# mu0: prior mean intercept
# varmu0: prior variance intercept
# ar0: prior mean ARs
# varar0: prior variance ARs
# ma0: prior mean MAs
# varma0: prior variance MAs
# a0/2: shape parameter variance of the model
# d0/2: rate parameter variance of the model
# mcmc: MCMC iterations
# burnin: burn-in iterations
# thin: thin parameter

# Output: ARMA models
# mu: Posterior chain intercept --> To download
# sigma: Posterior chain sigma --> To download
# AR: Posterior chain AR coefs --> To download
# MA: Posterior chain MA coefs --> To download
# SummaryAR: Summary AR coefs --> To display in GUI
# ARTestgeweke: Geweke test --> To display in GUI
# ARTestraftery: Raftery test --> To display in GUI
# ARTestheidel: Heidel test --> To display in GUI
# SummaryMA: Summary MA coefs --> To display in GUI
# MATestgeweke: Geweke test --> To display in GUI
# MATestraftery: Raftery test --> To display in GUI
# MATestheidel: Heidel test --> To display in GUI
# Summary: Summary intercept y sigma --> To display in GUI
# TestsVarMu: Geweke, Raftery and Heidel tests intercept --> To display in GUI
# TestsVarSigma: Geweke, Raftery and Heidel tests sigma --> To display in GUI
# Plotsmu: Plot mu posterior density --> To download
# Plotsmutrace: Plot mu posterior trace --> To download
# Plotsmucorr: Plot mu posterior autocorrelation --> To download
# PlotsSigma: Plot sigma posterior density --> To download
# PlotsSigmatrace: Plot sigma posterior trace --> To download
# PlotsSigmacorr: Plots sigma posterior autocorrelation --> To download
# PlotsAR: Plots AR posterior density --> To download
# PlotsARtrace: Plots AR posterior trace --> To download
# PlotsARcorr: Plots AR posterior autocorrelation --> To download
# PlotsMA: Plots MA posterior density --> To download
# PlotsMAtrace: Plots MA posterior trace --> To download
# PlotsMAcorr: Plots MA posterior autocorrelation --> To download

##### Stochastic volatility Models ########
dataSVM <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/63SimStochasticVolatility.csv")
source("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/Models/SVM.R")
ResSVM <- SVM(y = dataSVM[,1], X = dataSVM[,-1], MCMC = 10000, burnin = 1000, thin = 1, mu0 = 0, sdmu0 = 100, sigma0 = 1, a0 = 5, d0 = 1.5, b0 = 0, B0 = 10000)
# Input: Stochastic volatility models
# y: Dependent variable
# X: Regressors
# MCMC: Number of MCMC draws
# burnin: Number of burnin
# thin: Thining parameter
# mu0: Prior mean mu
# sdmu0: Prior standard deviation mu
# sigma0: Scaling parameter of  transformed variance
# a0: Shape parameter of prior of the transformed parameter phi
# d0: Scale parameter of prior of the transformed parameter phi
# b0: Prior mean coefficients of regressors
# B0: Prior standard deviation coefficients of regressors

# Output: Stochastic volatility models
# PostSVpar: Posterior chain SV parameters --> To download
# PostBetas: Posterior chain Betas --> To download
# MeanStates: Mean states --> To download
# LimInferiorStates: Inferior limit states --> To download
# LimSuperiorStates: Superior limit states --> To download
# PlotSV: Plot SV --> To display in GUI
# Summary: Summary posterior chains fixed parameters --> To display in GUI
# TestsSVpar: Diagnostics of SV parameters --> To display in GUI
# TestsBetas: Diagnostics of Betas --> To display in GUI

##### VAR Models ########
dataVAR <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/64SimVARmodels.csv")
source("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/Models/VAR.R")
ResVAR <- VAR(Y = dataVAR, p = 1, k0 = 2, k1 = 0.5, k3 = 5, H1 = 10, H2 = 4, type = "feir", cum = TRUE, MCMC = 10000, burnin = 1000, thin = 1)
# Input: VAR models
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

# Output: VAR models
# ImpulseResonse: Impulse response results --> To download
# ImpulseResonsePlots: Impulse response functions --> To display in GUI
# Forecasts: Forecasts --> To download
# ForecastPlots : Forecast plots --> To display in GUI

