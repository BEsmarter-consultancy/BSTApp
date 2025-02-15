##Upload user data
fileBMA<- fileInput('fileBMA', 'Choose File',
                    accept=c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv'))

filechBMA<- checkboxInput('headerBMA', 'Header', TRUE)


rbBMA<- radioButtons('sepBMA', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     selected=',')
uploadBMA=fluidRow(column(6,fileBMA),column(3,filechBMA),column(3,rbBMA))


fileBMAI<- fileInput('fileBMAI', 'Choose File (Instruments)',
                     accept=c('text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv'))

filechBMAI<- checkboxInput('headerBMAI', 'Header', TRUE)


rbBMAI<- radioButtons('sepBMAI', 'Separator',
                      c(Comma=',',
                        Semicolon=';',
                        Tab='\t'),
                      selected=',')
uploadBMAI=fluidRow(column(6,fileBMAI),column(3,filechBMAI),column(3,rbBMAI))



#Dynamic Models

fileBMADM<- fileInput('fileBMADM', 'Choose File (Model Matrix)',
                     accept=c('text/csv',
                              'text/comma-separated-values,text/plain',
                              '.csv'))

filechBMADM<- checkboxInput('headerBMADM', 'Header', TRUE)


rbBMADM<- radioButtons('sepBMADM', 'Separator',
                      c(Comma=',',
                        Semicolon=';',
                        Tab='\t'),
                      selected=',')
uploadBMADM=fluidRow(column(6,fileBMADM),column(3,filechBMADM),column(3,rbBMADM))




numEnd=numericInput('numEnd', 'Number of Endogenous variables', value=1, min = 1, max = 20, step = 1)

par1DBMA = numericInput('par1DBMA','Forgetting parameter of covariance matrix (lambda)', value=0.99, max =1, min =0)
par2DBMA = numericInput('par2DBMA','Forgetting parameter transition model probabilities(delta)', value=0.99, max =1, min =0)

summaryBMA=dataTableOutput('summaryBMA')
summaryBMA2=dataTableOutput('summaryBMA2')
summaryBMA3=plotOutput('summaryBMA3')

#conditional for normal models

normaltcond<- uiOutput("normaltcond")

normalT=radioButtons("normalT", "Which type do you want to perform?",
                     c("BIC"="1",
                       "MC3"="2",
                       "Intrumental variable"="3",
                       "Dynamic BMA"="4")
)
goBMAN1<- actionButton("goBMAN1", "Go!")
goBMAN2<- actionButton("goBMAN2", "Go!")
goBMAN3<- actionButton("goBMAN3", "Go!")
goBMAN4<- actionButton("goBMAN4", "Go!")

BMA_OR=numericInput('BMA_OR','OR: Number between 5 and 50.',value = 50,min = 5)

BMA_OL=numericInput('BMA_OL','OL: Number between 0.0001 and 1',value = 0.0025,min = 0.0001, max=1)

CNBMA=fluidPage( normalT,uploadBMA,normaltcond)



#conditional for logit models

goBMAL<- actionButton("goBMAL", "Go!")
CLBMA=fluidPage(uploadBMA,
                helpText(base_help,'52SimLogitBMA.csv'),
                br(),fluidRow(column(3,BMA_OR),column(3,BMA_OL)),h6("Logit"),goBMAL,br(),br(),summaryBMA)

#conditional for Poisson models
goBMAP<- actionButton("goBMAP", "Go!")
CPBMA=fluidPage(uploadBMA,
                helpText(base_help,'54SimPoissonBMA.csv'),
                br(),fluidRow(column(3,BMA_OR),column(3,BMA_OL)),h6("Poisson"),goBMAP,br(),br(),summaryBMA)

#conditional for GAMMA models
goBMAG<- actionButton("goBMAG", "Go!")
CGBMA=fluidPage(uploadBMA,
                helpText(base_help,'53SimGammaBMA.csv'),
                br(),fluidRow(column(3,BMA_OR),column(3,BMA_OL)),h6("Gamma"),goBMAG,br(),br(),summaryBMA)

DLBIC<- downloadButton('DLBIC', 'Download results using BIC')
DLMC3<- downloadButton('DLMC3', 'Download results using MC3')
DLMC3en<- downloadButton('DLMC3en', 'Download results using IV')
DLDyn<- downloadButton('DLDyn', 'Download results for Dynamic Model Average')
normalDW<- uiOutput("normalDW")



  ##CONDITIONALS UI
  output$normalDW <- renderUI({

    switch(input$normalT,
           "1"=DLBIC,
           "2"=DLMC3,
           "3"=DLMC3en,
           "4"=DLDyn
    )

  })





  output$CONDBMA <- renderUI({
    switch(input$radioBMA,
           "NS"=fluidPage(),
           "NBMA"=fluidPage(CNBMA,br(),br(),normalDW),
           "LBMA"=fluidPage(CLBMA,br(),br(),DLBIC),
           "GBMA"=fluidPage(CGBMA,br(),br(),DLBIC),
           "PBMA"=fluidPage(CPBMA,br(),br(),DLBIC)
    )
  })



  output$normaltcond <- renderUI({

    switch(input$normalT,
           "1"=fluidPage(helpText(base_help,'511SimNormalBMA.csv'),
                         br(),fluidRow(column(3,BMA_OR)),h6("Using BIC approximation: Be patient! This can take time."),
                         goBMAN1,
                         br(),br(),br(),summaryBMA),
           "2"=fluidPage(
                         helpText(base_help,'512SimNormalBMA.csv'),
                         fluidRow(column(6,itBMAMC3)),
                         h6("Performing MC3: Be patient! This can take time."),
                         goBMAN2,
                         br(),br(),br(),summaryBMA),
           "3"=fluidPage(helpText(base_help,'513SimNormalBMAivYXW.csv'),
                          uploadBMAI,
                         helpText(base_help,'513SimNormalBMAivZ.csv'),
                         numEnd,h6("Intrumental variable setting:"),
                         withMathJax(helpText(" $$Y=X\\beta_{End} +W\\beta_{Ex} + \\epsilon_Y$$"),
                                     helpText(" $$X=Z\\gamma_Z + W\\gamma_W + \\epsilon_X$$"),
                                     helpText(" First file should include a matrix [Y X W], where Y are main interest variables, X are endogeneous regressors, and W are exogenous regressors. Second file includes only the instruments (Z).")),
                         fluidRow(column(6,itBMA),column(6,it2BMA)),
                         goBMAN3, helpText("Performing Gibbs sampling: Be patient! This can take time."),
                         summaryBMA2,br(),br(),br(),summaryBMA
           ),
           "4"=fluidPage(helpText(base_help,'55SimDynamicBMA.csv'),
                         uploadBMADM,
                         helpText(base_help,'55SimModels.csv'),
                         h6("Dynamic BMA set up"),
                         fluidRow(column(6,par1DBMA),column(6,par2DBMA)),
                         goBMAN4, 
                         summaryBMA3,br(),br()
           ),
    )
  })

  ####Lectura de datos
  dataInputBMA <- reactive({
    inFile1 <- input$fileBMA
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$headerBMA, sep=input$sepBMA)
  })
  dataInputBMAI <- reactive({
    inFile1 <- input$fileBMAI
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$headerBMAI, sep=input$sepBMAI)
  })
  dataInputBMADM <- reactive({
    inFile1 <- input$fileBMADM
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$headerBMADM, sep=input$sepBMADM)
  })


  #####variable de comunicacion
  rvBMA <- reactiveValues(
    results=NULL,
    results2=NULL,
    obj=NULL,
    objBIC=NULL,
    objMC3=NULL,
    objEN=NULL,
    type=NULL
  )

  output$DLBIC <- downloadHandler(
    filename = 'BIC.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("Descriptive Statistics Coefficients.csv", "Best Models.csv")
      obj=rvBMA$obj
      aux=summary(obj)
      aux11=matrix(as.numeric(aux[1:(nrow(aux)-5),1:3]),ncol=3)
      colnames(aux11)=c("p!=0","EV","SD")
      rownames(aux11)=rownames(aux)[1:(nrow(aux)-5)]
      write.csv(aux11, file = fs[1])
      RES<-cbind(obj$which,obj$postprob)
      rownames(RES)<-paste("Model",1:nrow(obj$which))
      colnames(RES)<-c(paste("x",1:ncol(obj$which)),"PostProb")
      write.csv(RES, file = fs[2])
      print (fs)

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )

  output$DLMC3 <- downloadHandler(
    filename = 'MC3.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("Descriptive Statistics Coefficients.csv", "Best Models.csv")

      write.csv(rvBMA$results, file = fs[1])
      obj=rvBMA$objMC3
      aux=cbind(obj$variables,obj$post.prob)
      colnames(aux)=c(colnames(obj$variables),"PostProb")
      rownames(aux)=paste('Model',1:nrow(aux))
      write.csv(aux, file = fs[2])
      print (fs)

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )

  output$DLMC3en <- downloadHandler(
    filename = 'MC3iv.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("BMA Results First Stage.csv", "BMA Results Second Stage.csv","Posterior chains.csv")

      write.csv(rvBMA$results, file = fs[1])
      write.csv(rvBMA$results2, file = fs[2])
      obj=rvBMA$objEN
      rho=t(obj$rho)
      nombreR=paste('beta',1:ncol(obj$rho))
      lambda=NULL
      Sigma=NULL
      nombreL=NULL
      nombreS=NULL
      for(j in 1:dim(obj$lambda)[2]){
        lambda=rbind(lambda,obj$lambda[,j,])
        nombreL=c(nombreL,paste("gamma",j,1:nrow(obj$lambda[,1,])))
        for(l in j:(dim(obj$lambda)[2]+1)){
          Sigma=rbind(Sigma,obj$Sigma[j,l,])
          nombreS=c(nombreS,paste("Sigma",j,l))
        }
      }
      aux=rbind(rho,lambda,Sigma,obj$Sigma[dim(obj$lambda)[2]+1,dim(obj$lambda)[2]+1,])
      rownames(aux)=c(nombreR,nombreL,nombreS,paste("Sigma",dim(obj$lambda)[2]+1,dim(obj$lambda)[2]+1))
      write.csv(aux, file = fs[3])
      print (fs)

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )


  output$DLDyn <- downloadHandler(
    filename = 'DynamicBMA.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())



      results = rvBMA$resultDBMA
      # PMP: Dynamic posterior model probability --> To download
      # BetasBMAmean: Posterior BMA means --> To download
      # BetasBMAsd: Posterior BMA standard deviation --> To download
      # ForecastsMean: BMA forecast --> To download
      fs <- c("Posterior model Probabilities.csv",
              "Posterior BMA mean.csv",
              "Posterior BMA standard deviation.csv",
              "BMA forecast.csv")

      write.csv(results$PMP, file = fs[1])
      write.csv(results$BetasBMAmean, file = fs[2])
      write.csv(results$BetasBMAsd, file = fs[3])
      write.csv(results$ForecastsMean, file = fs[4])

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )




  output$summaryBMA <- renderDataTable({
    if(!is.null(rvBMA$results)){

      A=rvBMA$results

      dt=datatable(A,options = list(
        pageLength = nrow(A)),caption = rvBMA$type)
      dt

    }else{
      a=matrix(c("No results yet","Upload data and click the go button"))
      dt=datatable(a)
      dt


    }

  })

  output$summaryBMA2 <- renderDataTable({
    if(!is.null(rvBMA$results2)){

      A=rvBMA$results2

      dt=datatable(A,options = list(
        pageLength = nrow(A)),caption = "Gaussian family Instrumental variable (Objective model)")
      dt

    }else{


    }

  })


  output$summaryBMA3 <- renderPlot({
    print('ENTER SUMMARY')
    if(!is.null(rvBMA$resultDBMA)){

      A=rvBMA$resultDBMA

      print(A)
      print('kuaslfhlskduhsdlikuh')

      A$PlotPMP

    }else{


    }

  })


  observeEvent(input$goBMAN1, {
    showNotification("Working on it. Running greedy algorithm", duration = 60)

    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gaussian family using BIC approximation"
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bicreg(x=X, y=Y, strict = FALSE, OR = input$BMA_OR, maxCol = (hasta+1))
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
      rvBMA$objBIC=aux
    }
  })
  observeEvent(input$goBMAN2, {
    showNotification("Working on it. Running MC3 algorithm", duration = 60)
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gaussian family using MC3"
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      y=Y
      x=X
      print(sapply(X,mean))
      print(mean(Y))
      # ayuda=MC3.REG(Y, X, num.its=input$itBMAMC3,outs.list=NULL, outliers = FALSE )
      #
      # pmp=ayuda$post.prob
      #
      # which=ayuda$variables
      #
      # nModels=ayuda$n.models
      #
      # nreg=ncol(x)
      # betaModels=matrix(rep(0,nreg*nModels),nModels,nreg)
      #
      # BMAbeta=rep(0,nreg)
      # BMAvar=rep(0,nreg)
      # PIP=BMAbeta
      #
      # for (i in 1:nModels){
      #   included=which[i,]
      #   xred=as.matrix(x[,included])
      #
      #   model=lm(y ~ 0+xred)
      #   beta=model$coefficients
      #   betaModels[i,included]=beta
      #   BMAbeta[included]=BMAbeta[included]+beta*pmp[i]
      #   PIP[included]=PIP[included]+pmp[i]
      #
      #   ###This is following the formula for the variance
      #   esd=coef(summary(model))[, "Std. Error"]
      #   esd=esd^2
      #   suma=esd+beta^2
      #   BMAvar[included]=BMAvar[included]+suma*pmp[i]
      # }
      #
      # BMAvar=BMAvar-BMAbeta^2
      #
      # BMAsd=(BMAvar^0.5)

      # THIS IS NEW!!!!

      BMAreg=MC3.REG(Y, X, num.its=input$itBMAMC3,outs.list=NULL, outliers = FALSE )
      Models <- unique(BMAreg[["variables"]])
      nModels <- dim(Models)[1]
      nVistModels <- dim(BMAreg[["variables"]])[1]
      K <- ncol(x)
      PMP <- NULL
      for(m in 1:nModels){
        idModm <- NULL
        for(j in 1:nVistModels){
          if(sum(Models[m,] == BMAreg[["variables"]][j,]) == K){
            idModm <- c(idModm, j)
          }else{
            idModm <- idModm
          }
        }
        PMPm <- sum(BMAreg[["post.prob"]][idModm])
        PMP <- c(PMP, PMPm)
      }
      PMP

      PIP <- NULL
      for(k in 1:K){
        PIPk <- sum(PMP[which(Models[,k] == 1)])
        PIP <- c(PIP, PIPk)
      }
      plot(PIP)
      Means <- matrix(0, nModels, K)
      Vars <- matrix(0, nModels, K)
      for(m in 1:nModels){
        idXs <- which(Models[m,] == 1)
        if(length(idXs) == 0){
          Regm <- lm(y ~ 1)
        }
        Xm <- as.matrix(x[, idXs])
        Regm <- lm(y ~ Xm)
        SumRegm <- summary(Regm)
        Means[m, idXs] <- SumRegm[["coefficients"]][-1,1]
        Vars[m, idXs] <- SumRegm[["coefficients"]][-1,2]^2
      }
      BMAbeta <- colSums(Means*PMP)
      BMAsd <- (colSums(PMP*Vars)  + colSums(PMP*(Means-matrix(rep(BMAbeta, each = nModels), nModels, K))^2))^0.5
      #############################

      table=cbind(round(PIP*100,1),formatC(BMAbeta, format = "e", digits = 3),round(BMAsd,6))
      colnames(table)=c("p!=0","EV","SD")
      rownames(table)=colnames(x)

      rvBMA$results=table
      rvBMA$objMC3=BMAreg
    }
  })


  observeEvent(input$goBMAN3, {
    showNotification("Working on it. Running MCMC sampling", duration = 60)
    if (is.null(dataInputBMA())||is.null(dataInputBMAI())){
      return()
    }else{
      rvBMA$type="Gaussian family Instrumental variable (Instrumental variable stage)"
      YX=dataInputBMA()
      Y=YX[,1]
      numEndo=input$numEnd

      X=YX[,2:(1+numEndo)]
      W=YX[,-(1:(1+numEndo))]
      Z=dataInputBMAI()

      aux <- ivbma(Y, X, Z, W, s = input$itBMA+input$it2BMA, b = input$it2BMA, odens = input$itBMA)

      PIP2=aux$M.bar
      exp2=aux$lambda.bar

      tabla2 = PIP2[,1]
      cols=""
      for (i in 1 :ncol(PIP2)){
        cols=c(cols,paste("p!=0",i), paste("EV",i))
        tabla2=cbind(tabla2,round(PIP2[,i]*100,1),formatC(exp2[,i], format = "e", digits = 3))
      }
      tabla2=tabla2[,-1]


      colnames(tabla2)=cols[-1]
      rownames(tabla2)=c(colnames(Z),colnames(YX)[-(1:(1+numEndo))])


      PIP=aux$L.bar
      exp=aux$rho.bar

      tabla1=cbind(round(PIP*100,1),formatC(exp, format = "e", digits = 3))

      colnames(tabla1) = c("p!=0","EV")
      rownames(tabla1) = colnames(YX)[-1]
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=tabla2
      rvBMA$results2=tabla1
      rvBMA$objEN=aux
    }
  })




  observeEvent(input$goBMAL, {
    showNotification("Working on it. Running greedy algorithm", duration = 60)
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Logistic using BIC approximation"
      if ((sum(dataInputBMA()[,1]==1) +sum(dataInputBMA()[,1]==0))<length(dataInputBMA()[,1])){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      print(head(X))
      print(cov(X))
      print(solve(cov((X))))
      print(head(X))
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR, OR.fix = -log(input$BMA_OL)/log(input$BMA_OR), maxCol = (hasta+1), glm.family=binomial())
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))

    }
  })
  observeEvent(input$goBMAP, {
    showNotification("Working on it. Running greedy algorithm", duration = 60)
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Poisson using BIC approximation"
      if (sum(is.integer(dataInputBMA()[,1]) )<1){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR, OR.fix = -log(input$BMA_OL)/log(input$BMA_OR), maxCol = (hasta+1), glm.family=poisson())
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
    }
  })
  observeEvent(input$goBMAG, {
    showNotification("Working on it. Running greedy algorithm", duration = 60)
    if (is.null(dataInputBMA())){
      return()
    }else{
      rvBMA$type="Gamma using BIC approximation"
      if (sum(dataInputBMA()[,1]>=0)<length(dataInputBMA()[,1])){
        rvBMA$results<-matrix("Responce variable is not as expected")
        return()
      }
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      hasta=dim(X)[2]
      aux <- bic.glm(x=X, y=Y, strict = FALSE, OR = input$BMA_OR, OR.fix = -log(input$BMA_OL)/log(input$BMA_OR), maxCol = (hasta+1),  glm.family=Gamma(link="log"))
      nv=dim(YX)[]
      rvBMA$obj=aux
      rvBMA$results=as.matrix(summary(aux))
    }
  })

  observeEvent(input$goBMAN4, {

    showNotification("Working on it. ", duration = 5)
    if (is.null(dataInputBMA())||is.null(dataInputBMADM())){
      return()
    }else{
      rvBMA$type="Dynamic BMA"
      YX=dataInputBMA()
      Y=YX[,1]
      X=YX[,-1]
      models = dataInputBMADM()

      ResDBMA <- DBMA(y = Y, X = X, models = models, lambda = input$par1DBMA , delta = input$par2DBMA)


      rvBMA$resultDBMA=ResDBMA
    }
  })

