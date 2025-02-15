output$time_seriesUI <- renderUI({

  switch(input$Mts,
         "0"=fluidPage(),
         "DML"=fluidPage(dlm_row1,dlm_row2,
         actionButton('dlmpre', 'Pre calculate Prior'),
          actionButton('dlmgo', 'Go!'),
          br(),
          h3('States Plot'),
          plotOutput('dlm_plot1'),
          br(),
          verbatimTextOutput('dlm_print'),
          br(),
          downloadButton('dlmdwd', 'Download results')

           ),
         "ARMA"=fluidPage(arma_row1,arma_row2,arma_row3,arma_row4,arma_row5,actionButton('armago','Go!'), verbatimTextOutput('arma_print'), downloadButton('armadwd','Download results')



          ),
         "SVM"=fluidPage(svm_row1,svm_row2,svm_row3,actionButton('svmgo','Go!'), h3('Stochastic volatility Plot'),plotOutput('svm_plot1'), br(),verbatimTextOutput('svm_print'),br(), downloadButton('svmdwd','Download results')



         ),
         "VAR"=fluidPage(var_row1,var_row2,var_row3,actionButton('vargo','Go!'), h3('Impulse Resonse Plot'),plotOutput('var_plot1'), br(),br(), h3('Forecast Plot'),plotOutput('var_plot2'), downloadButton('vardwd','Download results')



         ),
  )
})


output$time_seriesHT <- renderUI({

  switch(input$Mts,
         "0"=fluidPage(),
         "DML"=helpText(base_help,'61SimDynamicLinearModel.csv'),
         "ARMA"=helpText(base_help,'62ARMAModels.csv'),
         "SVM"=helpText(base_help,'63SimStochasticVolatility.csv'),
         "VAR"=helpText(base_help,'64VARModels.csv')
  )
})


dataInput_ts <- reactive({
  inFile1 <- input$file6
  if (is.null(inFile1))
    return(NULL)
  read.csv(inFile1$datapath, header=input$header6, sep=input$sep6)
})

rv_ts = reactiveValues()
### ARMA
observeEvent(input$armago, {

  data = dataInput_ts()

  if(is.null(data)) {
      showNotification("Please upload data.", type = "error")
      return(NULL)
    }

  showNotification("Working on it. Starting", duration = 5)
  ResARMA <- ARMA(y = data[,1],
                  f = as.numeric(input$arma_f),
                  p = input$arma_p,
                  q = input$arma_q,
                  mu0 = input$arma_mu0,
                  varmu0 = input$arma_varmu0,
                  ar0 = input$arma_ar0,
                  varar0 = input$arma_varar0,
                  ma0 = input$arma_ma0,
                  varma0 = input$arma_varma0,
                  a0 = input$arma_a0,
                  d0 = input$arma_d0,
                  mcmc = input$it_ts,
                  burnin = input$burnin_ts,
                  thin = as.numeric(input$keep_ts))

  showNotification("Creating output", duration = 5)


  unlink(file.path(path,"Results"),recursive=TRUE)
  dir.create(file.path(path,"Results"),showWarnings = FALSE)
  setwd(file.path(path,"Results"))

  for (name in (names(ResARMA))){
    if (grepl('Plot',name,fixed = TRUE)){
      pdf(paste0(name,".pdf"))
      print(ResARMA[name])
      dev.off()
      setEPS()
      postscript(paste0(name,".eps"))
      print(ResARMA[name])
      dev.off()
    }


    if (name %in% c("mu","sigma","AR","MA")) {
      write.csv(ResARMA[name],paste0(name,".csv"))

    }
  }
  rv_ts$ResARMA = ResARMA

  setwd("..")

  showNotification("Done", duration = 5)



})

output$arma_print= renderPrint({
  if(!is.null(rv_ts$ResARMA)){
    res = rv_ts$ResARMA
    cat("Summary AR coefs")

    print(res$SummaryAR)
    cat("Geweke test")
    print(res$ARTestgeweke)
    cat("Raftery test")
    print(res$ARTestraftery)
    cat("Heidel test")
    print(res$ARTestheidel)
    cat("Summary MA coefs")
    print(res$SummaryMA)
    cat("Geweke test")
    print(res$MATestgeweke)
    cat("Raftery test")
    print(res$MATestraftery)
    cat("Heidel test")
    print(res$MATestheidel)


    cat("Summary Intercept")
    print(res$Summary[[1]])
    cat("Summary Sigma")
    print(res$Summary[[2]])
    cat("Geweke, Raftery and Heidel tests intercept")
    print(res$TestsVarMu)
    cat("Geweke, Raftery and Heidel tests sigma")
    print(res$TestsVarSigma)
  }
})





output$armadwd <- downloadHandler(
  filename = function() {
    paste("ARMA Results", "zip", sep=".")
  },

  content = function(file) {
    if(is.null(rv_ts$ResARMA)) {
      showNotification("No results available. Please run the model first.", type = "error")
      return(NULL)
    }
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)


###VAR

observeEvent(input$vargo, {

  data = dataInput_ts()
  if(is.null(data)) {
      showNotification("Please upload data.", type = "error")
      return(NULL)
    }

  showNotification("Running", duration = 5)
  ResVAR <- VAR(Y = data,
                p = input$var_p,
                k0 = input$var_k0,
                k1 = input$var_k1,
                k3 =input$var_k3,
                H1 = input$var_h1,
                H2 = input$var_h2,
                type = "feir",
                cum = TRUE,
                MCMC = input$it_ts, burnin = input$burnin_ts, thin = as.numeric(input$keep_ts))

  showNotification("Creating output", duration = 5)





  unlink(file.path(path,"Results"),recursive=TRUE)
  dir.create(file.path(path,"Results"),showWarnings = FALSE)
  setwd(file.path(path,"Results"))

  for (name in (names(ResVAR))){
    if (name=="ImpulseResonsePlots"){

      for (i in 1:length(ResVAR["ImpulseResonsePlots"][[1]])){
        pdf(paste0(name,i,".pdf"))
        print(ResVAR["ImpulseResonsePlots"][[1]][i])
        dev.off()
        setEPS()
        postscript(paste0(name,i,".eps"))
        print(ResVAR["ImpulseResonsePlots"][[1]][i])
        dev.off()

      }

    }

    if (name=="ForecastPlots"){

      for (i in 1:length(ResVAR["ForecastPlots"][[1]])){
        pdf(paste0(name,i,".pdf"))
        print(ResVAR["ForecastPlots"][[1]][i])
        dev.off()
        setEPS()
        postscript(paste0(name,i,".eps"))
        print(ResVAR["ForecastPlots"][[1]][i])
        dev.off()

      }

    }


    if (name %in% c("ImpulseResonse","Forecasts")) {
      write.csv(ResVAR[name],paste0(name,".csv"))

    }
  }
  rv_ts$ResVAR = ResVAR

  showNotification("Done", duration = 5)

  setwd("..")



})


output$vardwd <- downloadHandler(
  filename = function() {
    paste("VAR Results", "zip", sep=".")
  },

  content = function(file) {
    if(is.null(rv_ts$ResVAR)) {
      showNotification("No results available. Please run the model first.", type = "error")
      return(NULL)
    }
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)


output$var_plot1= renderPlot({
  if(!is.null(rv_ts$ResVAR)){
    ggarrange(plotlist=rv_ts$ResVAR["ImpulseResonsePlots"][[1]])
  }
})

output$var_plot2= renderPlot({
  if(!is.null(rv_ts$ResVAR)){
    ggarrange(plotlist=rv_ts$ResVAR["ForecastPlots"][[1]])
  }
})


## SVM



observeEvent(input$svmgo, {

  data = dataInput_ts()


  if(is.null(data)) {
      showNotification("Please upload data.", type = "error")
      return(NULL)
    }


  ResSVM <- SVM(y = data[,1], X = data[,-1], MCMC = input$it_ts, burnin = input$burnin_ts, thin = as.numeric(input$keep_ts),
                mu0 = input$svm_mu0,
                sdmu0 = input$svm_sdmu0,
                sigma0 = input$svm_sigma0,
                a0 = input$svm_a0,
                d0 = input$svm_d0,
                b0 = input$svm_b0,
                B0 = input$svm_B0
               )


  unlink(file.path(path,"Results"),recursive=TRUE)
  dir.create(file.path(path,"Results"),showWarnings = FALSE)
  setwd(file.path(path,"Results"))
  print(1)
  for (name in (names(ResSVM))){



    if (name %in% c("PostSVpar","MeanStates","LimInferiorStates","LimSuperiorStates")) {
      write.csv(ResSVM[name],paste0(name,".csv"))

    }

    if (name %in% c("PostBetas")) {
      write.csv(ResSVM[name]$PostBetas[[1]],paste0(name,".csv"))

    }
  }
  rv_ts$ResSVM  = ResSVM
  print(2)

  setwd("..")



})


output$svmdwd <- downloadHandler(
  filename = function() {
    paste("SVM Results", "zip", sep=".")
  },

  content = function(file) {
    if(is.null(rv_ts$ResSVM)) {
      showNotification("No results available. Please run the model first.", type = "error")
      return(NULL)
    }
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)


output$svm_plot1= renderPlot({
  if(!is.null(rv_ts$ResSVM )){
    rv_ts$ResSVM$PlotSV
  }
})


output$svm_print= renderPrint({
  if(!is.null(rv_ts$ResSVM )){
    res = rv_ts$ResSVM
    cat("Summary")
    print(rv_ts$ResSVM$Summary )
    cat("Diagnostics of SV parameters")
    print(rv_ts$ResSVM$TestsSVpar )
    cat("Diagnostics of Betas")
    print(rv_ts$ResSVM$TestsBetas )

  }
})

# Summary: Summary posterior chains fixed parameters --> To display in GUI
# TestsSVpar: Diagnostics of SV parameters --> To display in GUI
# TestsBetas: Diagnostics of Betas --> To display in GUI


# DLM handlers

observeEvent(input$dlmpre, {
  data = dataInput_ts()
  if(is.null(data)) {
      showNotification("Please upload data.", type = "error")
      return(NULL)
    }

  # Get default priors
  DefaultPrior <- AuxDLMprior(y = data[,1], x = data[,-1])

  updateNumericInput(session, "dlm_ay", value = DefaultPrior[1])
  updateNumericInput(session, "dlm_by", value = DefaultPrior[2])
  updateNumericInput(session, "dlm_atheta", value = DefaultPrior[3])
  updateNumericInput(session, "dlm_btheta", value = DefaultPrior[4])


})



observeEvent(input$dlmgo, {
  data = dataInput_ts()

  if(is.null(data)) {
      showNotification("Please upload data.", type = "error")
      return(NULL)
    }

  showNotification("Running", duration = 5)
  # Run DLM with user inputs
  ResDLM <- DLM(y = data[,1],
                x = data[,-1],
                a.y = input$dlm_ay,
                b.y = input$dlm_by,
                a.theta = input$dlm_atheta,
                b.theta = input$dlm_btheta,
                MCMC = input$it_ts,
                burnin = input$burnin_ts,
                thin = as.numeric(input$keep_ts))

  showNotification("Creating output", duration = 5)


  unlink(file.path(path,"Results"),recursive=TRUE)
  dir.create(file.path(path,"Results"),showWarnings = FALSE)
  setwd(file.path(path,"Results"))


  for (name in (names(ResDLM ))){

    if (name %in% c('VarianceObs','VarianceStates','MeanStates','LimInferiorStates','LimSuperiorStates')) {
      write.csv(ResDLM[name],paste0(name,".csv"))

    }
  }
  setwd("..")

  # Save results
  rv_ts$ResDLM = ResDLM

  showNotification("Done", duration = 5)
})

# DLM plot output
output$dlm_plot1 = renderPlot({
  if(!is.null(rv_ts$ResDLM)){

    ggarrange(plotlist=rv_ts$ResDLM$PlotStates)

  }
})

# DLM text output
output$dlm_print = renderPrint({
  if(!is.null(rv_ts$ResDLM)){
    cat("Summary of posterior chains variances\n")
    print(rv_ts$ResDLM$Summary)
    cat("\nDiagnostics variance of observation equation\n")
    print(rv_ts$ResDLM$TestsVarObs)
    cat("\nDiagnostics variance of state equations\n")
    print(rv_ts$ResDLM$TestsVarStates)
  }
})

# DLM download handler
output$dlmdwd <- downloadHandler(
  filename = function() {
    paste("DLM Results", "zip", sep=".")
  },
  content = function(file) {
    if(is.null(rv_ts$ResDLM)) {
      showNotification("No results available. Please run the model first.", type = "error")
      return(NULL)
    }
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)


output$svmdwd <- downloadHandler(
  filename = function() {
    paste("SVM Results", "zip", sep=".")
  },

  content = function(file) {
    if(is.null(rv_ts$ResSVM)) {
      showNotification("No results available. Please run the model first.", type = "error")
      return(NULL)
    }
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)
