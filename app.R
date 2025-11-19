

dep=c("rsconnect" ,"shiny","mlogit" ,"MASS" ,"AER" ,
      "mcmc","bayesm",
      "abind","bayesboot","bibtex" ,
      "BMA","car","carData","cellranger" ,"clipr",
      "coda","DT","Formula","ivbma",
      "Matrix" ,"matrixcalc","RcppEigen",
      "Rdpack","rhandsontable" ,"statmod", "truncnorm"  ,
      "stringr","MCMCpack","dma",'dlm',"bvartools","stochvol","bayesforecast","ggpubr","latex2exp")



needed=dep[!dep%in%installed.packages()]

if (length(needed)>0){
  for (p in needed){
    install.packages(p)
  }
}

needed=dep[!dep%in%installed.packages()]


if (length(needed)>0){
  for (p in needed){
    if (p=="ivbma"){
      install.packages("https://cran.r-project.org/src/contrib/Archive/ivbma/ivbma_1.05.tar.gz", repos = NULL, type = "source")
    }else if (p=="MCMCpack"){
      # install.packages("https://cran.r-project.org/src/contrib/Archive/MCMCpack/MCMCpack_1.4-4.tar.gz", repos = NULL, type = "source")
      install.packages("https://github.com/BESmarter-consultancy/BSTApp/raw/master/MCMCpack_1.7-1.tar.gz", repos = NULL, type = "source")
    }else{
      install.packages(p)
    }
  }
}



library(shiny)
options(shiny.maxRequestSize=100*1024^2)
image<- img(src="logo.png", height = 200, width = "90%") #Local variable


###############################################
################### UI ########################


## Load General conditions, packages and other stuff necesary for run the app UI
source(file.path("genUI.R"),  local = TRUE)$value

tab_head=a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")
#tab_head=strong("BEsmarter",style = "color:light blue")
#tab_head="BEsmarter"
ui <- navbarPage(tab_head,windowTitle = "BEsmarter",

  source(file.path("ui", "presentation.R"),  local = TRUE)$value,
  source(file.path("ui", "univariate.R"),  local = TRUE)$value,
  source(file.path("ui", "multivariate.R"),  local = TRUE)$value,
  source(file.path("ui", "time_series.R"),  local = TRUE)$value,
  source(file.path("ui", "hierarchical.R"),  local = TRUE)$value,
  #source(file.path("ui", "nonpar.R"),  local = TRUE)$value,
  source(file.path("ui", "BMAGLM.R"),  local = TRUE)$value,

  source(file.path("ui", "help.R"),  local = TRUE)$value
)

#################################
########## Server ###############

## Load General conditions, packages and other stuff necessary for run the app
source(file.path("genServer.R"),  local = TRUE)$value


server <- function(input, output, session) {
# Include the logic (server) for each tab
  source(file.path("server", "presentation.R"),  local = TRUE)$value
  source(file.path("server", "univariate.R"),  local = TRUE)$value
  source(file.path("server", "multivariate.R"),  local = TRUE)$value
  source(file.path("server", "hierarchical.R"),  local = TRUE)$value
  #source(file.path("server", "nonpar.R"),  local = TRUE)$value
  source(file.path("server", "BMAGLM.R"),  local = TRUE)$value
  source(file.path("server", "time_series.R"),  local = TRUE)$value
  source(file.path("server", "help.R"),  local = TRUE)$value
  }

shinyApp(ui = ui, server = server)
