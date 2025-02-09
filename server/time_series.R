output$time_seriesUI <- renderUI({

  switch(input$Mts,
         "0"=fluidPage(),
         "DML"=fluidPage(

           ),
         "ARMA"=fluidPage(arma_row1,arma_row2,arma_row3,arma_row4,arma_row5,actionButton('armago','Go!'), verbatimTextOutput('arma_print'), downloadButton('armadwd','Download results')



          ),
         "SVM"=fluidPage(



         ),
         "VAR"=fluidPage(



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




