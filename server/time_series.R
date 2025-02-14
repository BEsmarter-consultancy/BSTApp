output$time_seriesUI <- renderUI({

  switch(input$Mts,
         "0"=fluidPage(),
         "DML"=fluidPage(

           ),
         "ARMA"=fluidPage(arma_row1,arma_row2,arma_row3,arma_row4,arma_row5,actionButton('armago','Go!'), verbatimTextOutput('arma_print'), downloadButton('armadwd','Download results')



          ),
         "SVM"=fluidPage(svm_row1,svm_row2,svm_row3,actionButton('svmgo','Go!'), h3('Impulse Resonse Plot'),plotOutput('svm_plot1'), br(),verbatimTextOutput('svm_print'),br(), downloadButton('svmdwd','Download results')



         ),
         "VAR"=fluidPage(var_row1,var_row2,var_row3,actionButton('vargo','Go!'), h3('Impulse Resonse Plot'),plotOutput('var_plot1'), br(),br(), h3('Forecast Plot'),plotOutput('var_plot2'), downloadButton('vardwd','Download results')



         ),
  )
})


output$time_seriesHT <- renderUI({

  switch(input$Mts,
         "0"=fluidPage(),
         "DML"=helpText(base_help,'DLM file'),
         "ARMA"=helpText(base_help,'ARMA file'),
         "SVM"=helpText(base_help,'SVM file'),
         "VAR"=helpText(base_help,'VAR file')
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



})

output$arma_print= renderPrint({
  if(!is.null(rv_ts$ResARMA)){
    res = rv_ts$ResARMA
    cat("Summary AR coefs")
    print(res$SummaryAR[[1]])
    cat("Geweke test")
    print(res$ARTestgeweke[[1]])
    cat("Raftery test")
    print(res$ARTestraftery[[1]])
    cat("Heidel test")
    print(res$ARTestheidel[[1]])
    cat("Summary MA coefs")
    print(res$SummaryMA[[1]])
    cat("Geweke test")
    print(res$MATestgeweke[[1]])
    cat("Raftery test")
    print(res$MATestraftery[[1]])
    cat("Heidel test")
    print(res$MATestheidel[[1]])


    cat("Summary Intercept and sigma")
    print(res$Summary[[1]])
    cat("Geweke, Raftery and Heidel tests intercept")
    print(res$TestsVarMu[[1]])
    cat("Geweke, Raftery and Heidel tests sigma")
    print(res$TestsVarSigma[[1]])
  }
})





output$armadwd <- downloadHandler(
  filename = function() {
    paste("ARMA Results", "zip", sep=".")
  },

  content = function(file) {
    zip(zipfile=file, files="Results")
  },
  contentType = "application/zip"
)


###VAR

observeEvent(input$vargo, {

  data = dataInput_ts()


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

  print('READY!')





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

  print('READY 2!')

  setwd("..")



})


output$vardwd <- downloadHandler(
  filename = function() {
    paste("VAR Results", "zip", sep=".")
  },

  content = function(file) {
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

  print(name)

    if (name %in% c("PostSVpar","MeanStates","LimInferiorStates","LimSuperiorStates")) {
      write.csv(ResSVM[name]$PostBetas[[1]],paste0(name,".csv"))

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


