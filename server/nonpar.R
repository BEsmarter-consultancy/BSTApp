  ######Data NavBar 4. Models #########
  dataInput42 <- reactive({
    inFile4 <- input$file4
    if (is.null(inFile4))
      return(NULL)
    read.csv(inFile4$datapath, header=input$header4, sep=input$sep4)
  })



  ##### 4.2 ###########
  output$nonpar <- renderUI({
    preview_and_aux <- aux_formula_ui('get_formula4',data=dataInput42())

    fluidPage(preview_and_aux,br(),
              actionButton("Ana4", "Build formula"),br(),
              fluidRow(column(3,FormulaM42),column(9,HTForm))
    )
  })

  get_model4 <- reactive({'M42'})

  formula_df4 <- aux_formula_server('get_formula4',model=get_model4,data=dataInput42)

  observeEvent(input$Ana4, {

    if (!is.null(dataInput42())){
      df <- formula_df4()
      new_formula <-  build_formula(df,'m42')
      updateTextInput(session, "Formula42", value=new_formula)

    }else{
      showNotification('Please upload data first')
    }

  })

  #############################################################################
  #############################################################################

  ######## 4.2 Models: Posterior Chains#########
  #
  Posteriors42 <- eventReactive(input$goButton42, {
    showNotification("Working on it. Runnig MCMC sampling: See progress bar!", duration = 60)

    lm.coefs <- function(dat){
      coef(lm(input$Formula42, data = dat))
    }
    print(input$itBB)
    print(input$BBr2)
    print(input$Formula42)
    bayesboot(dataInput42(), lm.coefs, R = input$itBB, R2=input$BBr2, use.weights = FALSE)
  })

  ####### 4.2 Models: Download Posterior Chains ########

  output$download42 <- downloadHandler(
    filename = function() {
      paste("Posterior Chains", '.csv', sep='')
    },

    content = function(file) {
      post42<- Posteriors42()
      write.csv(post42, file)
    }
  )

  ####### 4.2 Models: Summary Posterior Chains##########
  output$summary42 <- renderPrint({
    SumDiagBayBoots(Posteriors42())
  })

  ####### 4.2 Models: Summary Posterior Chains##########

  output$plot42 <- renderPlot({
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    dir.create(file.path(path,"Posterior Graphs"),showWarnings = FALSE)
    setwd(file.path(path,"Posterior Graphs"))

    graphs42<- function(post42){
      nc<-ncol(post42)
      for (i in 1:nc) {
        pdf(paste("Density Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot(post42[,i])
        dev.off()
        setEPS()
        postscript(paste("Density Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot(post42[,i])
        dev.off()
        pdf(paste("Trace Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.trace(post42[,i])
        dev.off()
        setEPS()
        postscript(paste("Trace Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.trace(post42[,i])
        dev.off()
        pdf(paste("Autocorrelation Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.corr(post42[,i])
        dev.off()
        setEPS()
        postscript(paste("Autocorrelation Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.corr(post42[,i])
        dev.off()
      }
    }
    graphs42(Posteriors42())
    setwd("..")
  })

  output$multiDownload42 <- downloadHandler(
    filename = function() {
      paste("Posterior Graphs", "zip", sep=".")
    },

    content = function(file) {
      zip(zipfile=file, files='Posterior Graphs')
    },
    contentType = "application/zip"
  )
