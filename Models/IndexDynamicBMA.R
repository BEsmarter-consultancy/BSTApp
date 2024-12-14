# Index: Time series
rm(list = ls())
library(dma)

##### Dynamic BMA ########
dataDBMA <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/55SimDynamicBMA.csv")
modelDBMA <- read.csv("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/DataSim/55SimModels.csv")
source("https://raw.githubusercontent.com/besmarter/BSTApp/refs/heads/master/Models/DBMA.R")
ResDBMA <- DBMA(y = dataDBMA[,1], X = dataDBMA[,-1], models = modelDBMA, lambda = 0.99, delta = 0.99)
# Input: Dynamic BMA
# y: Dependent variable
# X: Regressors
# models: Matrix of models, the variables are in the columns,
#         and the rows are model indicators (0/1)
# lambda: Forgetting parameter of covariance matrix
# delta: Forgetting parameter transition model probabilities
# y = y; X = X; models = combs; lambda = 0.99; gamma = 0.99

# Output: Dynamic BMA
# PMP: Dynamic posterior model probability --> To download
# BetasBMAmean: Posterior BMA means --> To download
# BetasBMAsd: Posterior BMA standard deviation --> To download
# ForecastsMean: BMA forecast --> To download
# PlotPMP: Plot of the posterior model probabilities in time --> To display in GUI
