## required packages
library(matrixcalc)
library(truncnorm)
library(shiny)
library(rhandsontable)
library(coda)
library(bayesm)
library(MCMCpack)
library(MASS)
library(bayesboot)
library(ivbma)
library(BMA)
library(DT)
library(Matrix)
library(AER)
library(Formula)
library(RcppEigen)
library(Rdpack)
library(bibtex)
library(car)
library(abind)
library(carData)
library(cellranger)
library(clipr)
library(mlogit)
library(statmod)


image<- img(src="logo.png", height = 200, width = "90%") #Local variable

##### 1. Univariate Models: First NavBar#####
file1m<- fileInput('file1', 'Choose File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv'))
filech1m<- checkboxInput('header1', 'Header', TRUE)
rb1m<- radioButtons('sep1', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################
##### 2. Multivariate Models: Second NavBar#####

file2m<- fileInput('file2', 'Choose File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv'))
filech2m<- checkboxInput('header2', 'Header', TRUE)
rb2m<- radioButtons('sep2', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')

#######################################################

##### 3. Longitudinal Models: Forth NavBar#####
file3m<- fileInput('file3', 'Choose File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv'))
filech3m<- checkboxInput('header3', 'Header', TRUE)
rb3m<- radioButtons('sep3', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################

##### 4. Non-Parametric Models: Forth NavBar#####
file4m<- fileInput('file4', 'Choose File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv'))
filech4m<- checkboxInput('header4', 'Header', TRUE)
rb4m<- radioButtons('sep4', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')
###################################

#####Univariate#################
it1<- sliderInput("it",
                  "MCMC iterations:",
                  value = 10000,
                  min = 10000,
                  max = 100000,
                  step = 10000)
it2<- sliderInput("burnin",
                  "Burn-in Size:",
                  value = 1000,
                  min = 1000,
                  max = 10000,
                  step = 1000)

it3<- selectInput("keep", "Thinning parameter:",
                  choices = c("1", "5", "10", "20", "50", "100"), selected = "1")

#####Multivariate#################
it1MV<- sliderInput("itMV",
                  "MCMC Iterations:",
                  value = 10000,
                  min = 10000,
                  max = 100000,
                  step = 10000)
it2MV<- sliderInput("burninMV",
                  "Burn-in Size:",
                  value = 1000,
                  min = 1000,
                  max = 10000,
                  step = 1000)

it3MV<- selectInput("keepMV", "Thinning parameter:",
                  choices = c("1","5", "10", "20", "50", "100"), selected = "1")

#####Hierarchical#################
it1HM<- sliderInput("itHM",
                    "MCMC Iterations:",
                    value = 10000,
                    min = 10000,
                    max = 100000,
                    step = 10000)
it2HM<- sliderInput("burninHM",
                    "Burn-in Size:",
                    value = 1000,
                    min = 1000,
                    max = 10000,
                    step = 1000)

it3HM<- selectInput("keepHM", "Thinning parameter:",
                    choices = c("1","5", "10", "20", "50", "100"), selected = "1")

######Nonparametric: Bootstrap########
it1BB<- sliderInput("itBB",
                  "MCMC Iterations:",
                  value = 10000,
                  min = 10000,
                  max = 100000,
                  step = 5000)

BBr2<- sliderInput("BBr2",
                    "Resampling Size:",
                    value = 1000,
                    min = 1000,
                    max = 10000,
                    step = 1000)


HT<- helpText("Click the button (Go!) after importing the dataset and selecting the model to update the value displayed in the main panel.")
BE<- helpText("Warning: Be patient this may take several minutes!!!")

######BMA########
itBMA<- sliderInput("itBMA",
                    "MCMC Iterations:",
                    value = 10000,
                    min = 10000,
                    max = 100000,
                    step = 5000)

it2BMA<- sliderInput("it2BMA",
                    "Burn-in Sample:",
                    value = 1000,
                    min = 1000,
                    max = 10000,
                    step = 1000)

itBMAMC3<- sliderInput("itBMAMC3",
                     "MC3 Iterations:",
                     value = 10000,
                     min = 10000,
                     max = 100000,
                     step = 5000)

##### 1.1 #########
DUI11<- uiOutput("ui11")
go11<- actionButton("goButton11", "Go!")
DL11<- downloadButton('download11', 'Download Posterior Chains')
DLP11<- downloadButton('multiDownload11', 'Download Posterior Graphs')
pplot11<- plotOutput("plot11", height = 1)

##### 2.1 #########
DUI21<- uiOutput("ui21")
go21<- actionButton("goButton21", "Go!")
DL21<- downloadButton('download21', 'Download Posterior Chains')
DLP21<- downloadButton('multiDownload21', 'Download Posterior Graphs')
pplot21<- plotOutput("plot21", height = 1)

##### 3.1 #########
DUI31<- uiOutput("ui31")
go31<- actionButton("goButton31", "Go!")
DL31<- downloadButton('download31', 'Download Posterior Chains')
DLP31<- downloadButton('multiDownload31', 'Download Posterior Graphs')
pplot31<- plotOutput("plot31", height = 1)

##### 4.2 #########
FormulaM42<- textInput("Formula42", "Main Equation", value = "")
DUI42<- uiOutput("ui42")
go42<- actionButton("goButton42", "Go!")
DL42<- downloadButton('download42', 'Download Posterior Chains')
DLP42<- downloadButton('multiDownload42', 'Download Posterior Graphs')
pplot42<- plotOutput("plot42", height = 1)


##### BMA GLM ####
radioBMA=radioButtons("radioBMA", "Bayesian Model Average",
                      c("No selection"="NS",
                        "Normal data"="NBMA",
                        "Binomial data (Logit)"="LBMA",
                        "Real positive data (Gamma)"="GBMA",
                        "Count data (Poisson)"="PBMA")
)

CONDBMA<- uiOutput("CONDBMA")

base_help = 'See Table 3 in our paper (Help tab) for template files to upload. You can also see the dataSim folder at rstudio.cloud in particular the file: '




##### 1. Univariate Models: First NavBar#####
file6m<- fileInput('file6', 'Choose File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv'))
filech6m<- checkboxInput('header6', 'Header', TRUE)
rb6m<- radioButtons('sep6', 'Separator',
                    c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                    selected=',')



## time series

it_ts<- sliderInput("it_ts",
                  "MCMC iterations:",
                  value = 10000,
                  min = 10000,
                  max = 100000,
                  step = 10000)
it2_ts<- sliderInput("burnin_ts",
                  "Burn-in Size:",
                  value = 1000,
                  min = 1000,
                  max = 10000,
                  step = 1000)

it3_ts<- selectInput("keep_ts", "Thinning parameter:",
                  choices = c("1", "5", "10", "20", "50", "100"), selected = "1")



it_row_ts = fluidRow(column(4,it_ts),column(4,it2_ts),column(4,it3_ts))

### ARMA

arma_row1 = fluidRow(selectInput("arma_f", "Frequency:",
                            choices = c("1", "4","12"), selected = "1"), helpText("times per year: 1 (annual data), 4 (quarterly data), 12 (monthly data)"))

arma_row2 = fluidRow(column(3,numericInput('arma_p','AR order', value = 1,min = 0, step=1)), column(3,numericInput('arma_q','MA order', value = 1,min = 0, step=1)))
arma_row3 = fluidRow(column(3,numericInput('arma_mu0','Prior Mean intercept', value = 0)),
                     column(3,numericInput('arma_varmu0','Prior Variance Intercept', value = 1)),
                     column(3,numericInput('arma_ar0','Prior Mean ARs', value = 0)),
                     column(3,numericInput('arma_varar0','Prior Variance ARs', value = 0.5^2,min=0.00001)))
arma_row4 = fluidRow(column(3,numericInput('arma_ma0','Prior Mean MAs', value = 0)),
                     column(3,numericInput('arma_varma0','Prior Variance MAs', value = 0.5^2,min=0.00001)))

                     #,numericInput('arma_ma0','Prior Mean MAs', value = 0),

arma_row5 = fluidRow(column(3,numericInput('arma_a0','Shape parameter variance of the model', value = 0.01)),
                     column(3,numericInput('arma_d0','Rape parameter variance of the model', value = 0.01)))






# a0/2: shape parameter variance of the model
# d0/2: rate parameter variance of the model
# mcmc: MCMC iterations
# burnin: burn-in iterations
# thin: thin parameter
