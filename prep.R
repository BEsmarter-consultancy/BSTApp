
dep=c("rsconnect" ,"shiny","mlogit" ,"MASS" ,"AER" ,
      "mcmc","bayesm",
      "abind","bayesboot","bibtex" ,
      "BMA","car","carData","cellranger" ,"clipr",
      "coda","DT","Formula","ivbma",
      "Matrix" ,"matrixcalc","RcppEigen",
      "Rdpack","rhandsontable" ,"statmod", "truncnorm"  ,
      "stringr","MCMCpack","dma","bvartools","stochvol","bayesforecast","ggpubr")

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
      install.packages("https://cran.r-project.org/src/contrib/Archive/MCMCpack/MCMCpack_1.4-4.tar.gz", repos = NULL, type = "source")
    }else{
      install.packages(p)
    }
  }
}
