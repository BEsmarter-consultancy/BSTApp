getRegs=function(f){ #function that get the variables given a formula
  if(f==""){
    return("null")
  }
  nv=unlist(gregexpr(pattern ='~',f))
  f=substr(f, nv+1,nchar(f))
  regs="hola"
  desde=1
  i=1
  while(i<=nchar(f)){
    if(substr(f, i,i)=='+'){
      regs=c(regs,substr(f, desde,i-1))
      desde=i+1
    }
    i=i+1
  }
  regs=c(regs,substr(f, desde,nchar(f)))

  regs=regs[-1]
  return(regs)
}

getRegs2=function(f){
  if(f==""){
    return("null")
  }
  nv=unlist(gregexpr(pattern ='~',f))
  f=substr(f, nv+1,nchar(f)-2)
  regs="hola"
  desde=1
  i=1
  while(i<=nchar(f)){
    if(substr(f, i,i)=='+'){
      regs=c(regs,substr(f, desde,i-1))
      desde=i+1
    }
    i=i+1
  }
  regs=c(regs,substr(f, desde,nchar(f)))

  regs=regs[-1]
  return(regs)
}

isRegs=function(f,rn){
  rnf=getRegs(f)
  bool=sum((rnf==rn)*1)
  return(bool==length(rn))
}

sim=function(DF){
  if(nrow(DF)>1){
    DF1=DF
    DF1[lower.tri(DF, diag = TRUE)]= 0
    DF=DF1+t(DF1)+diag(diag(as.matrix(DF)))
  }
  return(DF)
}

  ######Data NavBar 1. Models #########
  rv=reactiveValues(
    warningSDP=""
  )
  dataInput1 <- reactive({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    read.csv(inFile1$datapath, header=input$header1, sep=input$sep1)
  })

  ######Formulas NavBar 1. Models #########

  sumtextM1a <- reactive({
    model.formula(input$Formula1a,dataInput1())
  })

  sumtextM1b <- reactive({
    model.formula(input$Formula1b,dataInput1())
  })




  get_model <- reactive({input$M11})

  formula_df <- aux_formula_server('get_formula',model=get_model,data=dataInput1)


  ####### Output UI #####
  ##### 1.1 ########
  output$ui11 <- renderUI({
    if (input$M11=='m110'){
      return()}
    else{
      preview_and_aux <- aux_formula_ui('get_formula',data=dataInput1())

      model_specific <- switch(input$M11,
             "m111" = fluidPage(
                                isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmean"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvar"),
                                        fluidRow(column(3,Psh),column(3,Psc)),
                                        fluidRow(column(3,HTsh),column(3,HTsc)))))

             ,
             "m112" = isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmean"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvar"),
                                        LogitTune
                                        )),
             "m113" = isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmean"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvar"))),
             "m114" = isolate(wellPanel(fluidRow(column(3,MultPnn),column(6,FormulaM1B)),
                                        fluidRow(column(3,HTMultPnn), column(9,HTFormMP)),
                                        fluidRow(column(4,MultPy),column(4,MultPXA),column(4,MultPXD)),
                                        fluidRow(column(4,HTMultPy),column(4,HTMultPXA),column(4,HTMultPXD)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmeanLP"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvarLP"),
                                        helpText("Introduce scale matrix Inverse Wishart distribution"),
                                        rHandsontableOutput("hotPvarLP2"),
                                        fluidRow(column(3,PshIWMP)),
                                        fluidRow(column(3,HTshIWMP))
                                        )),
             "m115" = isolate(wellPanel(fluidRow(column(3,MultPnn),column(6,FormulaM1B)),
                                        fluidRow(column(3,HTMultPnn), column(9,HTFormMP)),
                                        fluidRow(column(4,MultPy),column(4,MultPXA),column(4,MultPXD)),
                                        fluidRow(column(4,HTMultPy),column(4,HTMultPXA),column(4,HTMultPXD)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmeanLP"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvarLP"),
                                        fluidRow(column(3,it4)),
                                        fluidRow(column(3,HTPtst))
                                        )),
             "m116" = isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTFormOP)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmean"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvar"),
                                        m116numAlt,
                                        helpText("Introduce prior mean for cut points"),
                                        rHandsontableOutput("hotCmean"),
                                        helpText("Introduce prior covariances for cut points"),
                                        rHandsontableOutput("hotCvar"),
                                        fluidRow(column(3,OprobitSCB),column(3,OprobitS),column(5,OprobitSHT))
                                        )),
             "m117" = isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                        helpText("Introduce prior mean vector location parameters"),
                                        rHandsontableOutput("hotPmean"),
                                        helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                        rHandsontableOutput("hotPvar"),
                                        fluidRow(column(3,Psh1),column(3,Psc1)),
                                        fluidRow(column(3,HTsh),column(3,HTsc)),
                                        fluidRow(column(3,NegBinAlpha))        ,
                                        fluidRow(column(3,NegBinBetaCB),column(3,NegBinBeta),column(5,NegBinBetaHT)) )),
             "m118" =isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                       fluidRow(column(3,Below),column(3,HTBelow),column(3,Above),column(3,HTAbove)),
                                       helpText("Introduce prior mean vector location parameters"),
                                       rHandsontableOutput("hotPmean"),
                                       helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                       rHandsontableOutput("hotPvar"),
                                       fluidRow(column(3,Psh),column(3,Psc)),
                                       fluidRow(column(3,HTsh),column(3,HTsc)))),
             "m119" =isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm)),
                                       fluidRow(column(3,tau),column(9,HTtau)),
                                       helpText("Introduce prior mean vector location parameters"),
                                       rHandsontableOutput("hotPmean"),
                                       helpText("Introduce prior covariances location parameters by row. It has to be symmetric"),
                                       rHandsontableOutput("hotPvar")
                                       )),
             "nonpar" =isolate(wellPanel(fluidRow(column(3,FormulaM1A),column(9,HTForm))

             ))
      )

      return(fluidPage(preview_and_aux,
                       actionButton("Ana1", "Build formula"),br(),
                      model_specific))
    }
  })

  observeEvent(input$Ana1, {

    if (!is.null(dataInput1())){
      df <- formula_df()
      new_formula <-  build_formula(df,input$M11)

      if(input$M11 %in% c('m114','m115')){

        xs1 <- rownames(df)[df$Type=='Alt Specific']

        xs2 <- rownames(df)[df$Type=='Not Alt Specific']



        p <- as.numeric(input$MultPLy)

        if (length(xs1)%%p!=0){
          new_formula <- 'Number of alternative specific regressors must be a multiple of the number of choice categorical alternatives'
        }

        updateTextInput(session, "MultPLXA", value=length(xs1)/p)
        updateTextInput(session, "MultPLXD", value=length(xs2))
        updateTextInput(session, "Formula1b", value=new_formula)
      }else{
        updateTextInput(session, "Formula1a", value=new_formula)
      }
    }else{
      showNotification('Please upload data first')
    }

  })

  output$hotPmean=renderRHandsontable({

    if(is.null(input$hotPmean) ){
      nv = 1
      f=input$Formula1a

      DF=data.frame("Prior mean"=rep(0,nv))
      if (input$M11=="m116"){
        rownames(DF)=getRegs2(f)
      }else{
        rownames(DF)=getRegs(f)
      }

    }else{
      DF=hot_to_r(input$hotPmean)
      if (input$M11=="m116"){
        rn=rownames(DF)
      }else{
        rn=rownames(DF)[-1]
      }
      f=input$Formula1a
      if(!identical(rn,getRegs(f))){
          f=input$Formula1a
          nv=unlist(gregexpr(pattern ='~',f))

          if(nv==-1){
            nv = 1
            DF=data.frame("Prior mean"=rep(0,nv))
            rownames(DF)="null"
          }else{

            if (input$M11=="m116"){
              regs=getRegs2(f)
              DF=data.frame("Prior mean"=rep(0,length(regs)))
              rownames(DF)=regs
            }else{
              regs=getRegs(f)
              DF=data.frame("Prior mean"=rep(0,length(regs)+1))
              rownames(DF)=c("cte",regs)
            }


          }

      }

    }
    DF$Prior.mean=as.numeric(DF$Prior.mean)

    rhandsontable(DF)%>%
      hot_col("Prior.mean",format="0.01")

  })

  output$hotPvar=renderRHandsontable({

    if(is.null(input$hotPvar) ){
      nv = 1
      f=input$Formula1a

      DF=data.frame("Prior mean"=0)
      if (input$M11=="m116"){
        rownames(DF)=getRegs2(f)
        colnames(DF)=getRegs2(f)
      }else{
        rownames(DF)=getRegs(f)
        colnames(DF)=getRegs(f)
      }

    }else{
      DF=hot_to_r(input$hotPvar)
      if (input$M11=="m116"){
        rn=rownames(DF)
      }else{
        rn=rownames(DF)[-1]
      }
      f=input$Formula1a
      if(!identical(rn,getRegs(f))){
        f=input$Formula1a
        nv=unlist(gregexpr(pattern ='~',f))

        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{

          regs=getRegs(f)

          if (input$M11=="m116"){
            DF=data.frame(diag(length(regs)))
            rownames(DF)=c(getRegs2(f))
            colnames(DF)=c(getRegs2(f))
          }else{
            DF=data.frame(diag(length(regs)+1))
            rownames(DF)=c("cte",getRegs(f))
            colnames(DF)=c("cte",getRegs(f))
          }

        }

      }

    }
    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
    rhandsontable(DF)%>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (col < row) {
               td.style.background = 'black';
               }
               }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")

    }

  })




  output$hotCvar=renderRHandsontable({

    if(is.null(input$hotCvar) ){
      nv = input$m116numAlt-2
      if(nv>0){
        DF=as.data.frame(diag(nv))
        rownames(DF)=paste("cut",1:nv)
        colnames(DF)=paste("cut",1:nv)
      }else{
        DF=data.frame("No cut points"=0)
      }

    }else{
      DF=hot_to_r(input$hotCvar)
      nc=ncol(DF)
      if (input$m116numAlt>2){
        if(colnames(DF)=="No.cut.points"){
          nv = input$m116numAlt-2
          DF=as.data.frame(diag(nv))
          rownames(DF)=paste("cut",1:nv)
          colnames(DF)=paste("cut",1:nv)
        }
        if(nc!=input$m116numAlt-2){
          nv = input$m116numAlt-2
          DF=as.data.frame(diag(nv))
          rownames(DF)=paste("cut",1:nv)
          colnames(DF)=paste("cut",1:nv)
        }
      }

    }
    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col < row) {
                 td.style.background = 'black';
                 }
                 }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")

    }

  })



  output$hotCmean=renderRHandsontable({

    if(is.null(input$hotCmean) ){
      nv = input$m116numAlt-2
      if(nv>0){
      DF=data.frame("Prior cut p"=rep(0,nv))
      DF$Prior.cut.p=as.numeric(DF$Prior.cut.p)
      rownames(DF)=paste("cut",1:nv)
      }else{
        DF=data.frame("No cut points"=0)
      }

    }else{
      DF=hot_to_r(input$hotCmean)
      nc=nrow(DF)
      if (input$m116numAlt>2){
        if(colnames(DF)=="No.cut.points"){
          nv = input$m116numAlt-2
          DF=data.frame("Prior cut p"=rep(0,nv))
          rownames(DF)=paste("cut",1:nv)

        }
        if(nc!=input$m116numAlt-2){
          nv = input$m116numAlt-2
          DF=data.frame("Prior cut p"=rep(0,nv))
          rownames(DF)=paste("cut",1:nv)

        }
      }
    }

    cn=colnames(DF)[1]
    if(cn=="Prior.cut.p"){
      DF$Prior.cut.p=as.numeric(DF$Prior.cut.p)
    rhandsontable(DF)%>%
      hot_col("Prior.cut.p",format="0.01")
    }else{
      DF$No.cut.points=as.numeric(DF$No.cut.points)
      rhandsontable(DF)%>%
        hot_col("No.cut.points",format="0.01")
    }

  })



  output$hotPmeanLP=renderRHandsontable({

    if(is.null(input$hotPmeanLP) ){
      p=as.numeric(input$MultPLy)
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)+1
      n=a1+ap*(p-1)
      f=input$Formula1b
      AS=paste('AS',1:a1,sep='_')
      NAS=1:p
      base=as.numeric(input$MultPLnn)
      NAS=NAS[-base]## quitar el base
      nas_names=paste('cte',NAS,sep='_')
      if(ap>1){
        for (extra in 2:ap){
          nas_names=c(nas_names,paste('NAS',extra-1,NAS,sep='_'))
        }
      }

      all_names=c(nas_names,AS)

      DF=data.frame(tit=rep(0,n))
      colnames(DF)=tit
      rownames(DF)=all_names

    }else{
      p=as.numeric(input$MultPLy)
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)+1
      n=a1+ap*(p-1)
      f=input$Formula1b
      AS=paste('AS',1:a1,sep='_')
      NAS=1:p
      base=as.numeric(input$MultPLnn)
      NAS=NAS[-base]## quitar el base
      nas_names=paste('cte',NAS,sep='_')
      if(ap>1){
        for (extra in 2:ap){
          nas_names=c(nas_names,paste('NAS',extra-1,NAS,sep='_'))
        }
      }

      all_names=c(nas_names,AS)

      DF=data.frame(tit=rep(0,n))
      colnames(DF)=tit
      #print(DF)
      #print(all_names)
      rownames(DF)=all_names
    }


    rhandsontable(DF)

  })

  output$hotPvarLP=renderRHandsontable({

    if(is.null(input$hotPvarLP) ){
      p=as.numeric(input$MultPLy)
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)+1
      n=a1+ap*(p-1)
      DF=data.frame(diag(n))
      colnames(DF)=1:n

    }else{
      p=as.numeric(input$MultPLy)
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)+1
      n=a1+ap*(p-1)
      DF=hot_to_r(input$hotPvarLP)
      if(nrow(DF)!=n){
        DF=data.frame(diag(n))
      }
      colnames(DF)=1:n
    }


    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col < row) {
                 td.style.background = 'black';
                 }
                 }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")

    }

  })


  output$hotPvarLP2=renderRHandsontable({

    if(is.null(input$hotPvarLP2) ){
      p=as.numeric(input$MultPLy)-1
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)
      n=a1+ap*(p-1)
      DF=data.frame(diag(p))
      colnames(DF)=1:p

    }else{
      p=as.numeric(input$MultPLy)-1
      tit="mean"
      if(is.na(p)){
        p=3
        tit="Check number of alternatives"
      }
      a1=as.numeric(input$MultPLXA)
      ap=as.numeric(input$MultPLXD)
      DF=hot_to_r(input$hotPvarLP2)
      if(nrow(DF)!=p){
        DF=data.frame(diag(p))
      }
      colnames(DF)=1:p
    }


    DF=sim(DF)
    x=as.matrix(DF)
    bool=is.positive.semi.definite(x, tol=1e-8)
    if(bool){
      rv$warningSDP=""
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (col < row) {
                 td.style.background = 'black';
                 }
                 }")
    }else{
      showNotification("Watch out! the red covariance matrix is not positive semi definite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")

    }

  })

  ######## 1.1 Models: Posterior Chains#########
  rv_current_result = reactiveValues(model = '')
  Posteriors11 <- eventReactive(input$goButton11, {


    rv_current_result$model = input$M11
    showNotification("Working on it. Runnig MCMC sampling", duration = 60)

    if(input$M11=='m111'){

      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

      if(input$PShL==""){a<-0.001}
      else{
        a<- isolate(as.numeric(input$PShL))
      }

      if(input$PScL==""){b<-0.001}
      else{
        b<- isolate(as.numeric(input$PScL))
      }
    }

    if(input$M11=='m112'){


      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

    }

    if(input$M11=='m113'){

      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

    }


    if(input$M11=='m114'){
      nn<- isolate(as.numeric(input$MultPLnn))
      pMP<- isolate(as.numeric(input$MultPLy))
      naMP<- isolate(as.numeric(input$MultPLXA))
      ndMP<- isolate(as.numeric(input$MultPLXD))
      XMP<- as.matrix(sumtextM1b()$X[,-1])
      if(naMP==0){Xa=NULL} else {Xa=as.matrix(XMP[,1:(pMP*naMP)])}
      if(ndMP==0){Xd=NULL} else {Xd=as.matrix(XMP[,(pMP*naMP+1):ncol(XMP)])}
      XMPP<- Xcreate(pMP, naMP, ndMP, Xa=Xa, Xd=Xd, INT = TRUE, DIFF = TRUE, base = nn)

      BmeanyMP<- hot_to_r(input$hotPmeanLP)[,1]
      BvaryMP<-solve(as.matrix(hot_to_r(input$hotPvarLP)))


      VMP<-as.matrix(hot_to_r(input$hotPvarLP2))

      if(input$PShLIWMP==""){nuMP<-NULL}
      else{
        nuMP<- isolate(as.numeric(input$PShLIWMP))
      }
    }

    if(input$M11=='m115'){
      nn<- isolate(as.numeric(input$MultPLnn))
      pMP<- isolate(as.numeric(input$MultPLy))
      naMP<- isolate(as.numeric(input$MultPLXA))
      ndMP<- isolate(as.numeric(input$MultPLXD))
      XMP<- as.matrix(sumtextM1b()$X[,-1])
      if(naMP==0){Xa=NULL} else {Xa=as.matrix(XMP[,1:(pMP*naMP)])}
      if(ndMP==0){Xd=NULL} else {Xd=as.matrix(XMP[,(pMP*naMP+1):ncol(XMP)])}
      XMPP<- Xcreate(pMP, naMP, ndMP, Xa=Xa, Xd=Xd, INT = TRUE, DIFF = FALSE, base = nn)

      BmeanyMP<- hot_to_r(input$hotPmeanLP)[,1]
      BvaryMP<-solve(as.matrix(hot_to_r(input$hotPvarLP)))

    }

    if(input$M11=='m116'){


      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))
      Bmeancut=NULL
      Bvarcut=NULL
      if(input$m116numAlt>2){
        Bmeancut=hot_to_r(input$hotCmean)[,1]
        Bvarcut=solve(as.matrix(hot_to_r(input$hotCvar)))
      }
    }

    if(input$M11=='m117'){

      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

      if(input$PShL==""){a<-0.001}
      else{
        a<- isolate(as.numeric(input$PShL))
      }

      if(input$PScL==""){b<-0.001}
      else{
        b<- isolate(as.numeric(input$PScL))
      }
    }


    if(input$M11=='m118'){


      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

      if(input$PShL==""){a<-0.001}
      else{
        a<- isolate(as.numeric(input$PShL))
      }

      if(input$PScL==""){b<-0.001}
      else{
        b<- isolate(as.numeric(input$PScL))
      }

      if(input$Below11==""){Be<-0}
      else{
        Be<- isolate(as.numeric(input$Below11))
      }

      if(input$Above11==""){Ab<-Inf}
      else{
        Ab<- isolate(as.numeric(input$Above11))
      }
    }

    if(input$M11=='m119'){

      Bmean<- hot_to_r(input$hotPmean)[,1]
      Bvar<- solve(as.matrix(hot_to_r(input$hotPvar)))

      if(input$tau11==""){t<-0.5}
      else{
        t<- isolate(as.numeric(input$tau11))
      }
    }

    MCMC<- list(R=input$it+input$burnin,keep=1,burnin=0,nprint=1000000)
    MCMCML<- list(R=input$it+input$burnin,keep=1,burnin=0,nu=as.numeric(input$nu),nprint=10000000)
    MCMCNB<-list(R=input$it+input$burnin,keep=1,burnin=0,nprint=1000000,s_beta=input$NegBinBeta,alpha=input$NegBinAlpha)

    if(input$M11=='m110')
      return()
    else {
      args <- switch(input$M11,
                     "m111" = list(form=input$Formula1a, data=dataInput1(), burnin = input$burnin+as.numeric(input$keep)-1, mcmc = input$it, thin=as.numeric(input$keep), verbose = 0, seed = NA, beta.start = NA, b0 = Bmean, B0 = Bvar, c0 = a, d0 = b),
                     "m112" = list(form=input$Formula1a, data=dataInput1(), burnin = input$burnin+as.numeric(input$keep)-1, mcmc = input$it, thin=as.numeric(input$keep), tune=input$LogitTune, verbose = 0, seed = NA, beta.start = NA, b0 = Bmean, B0 = Bvar),
                     "m113" = list(list(y=sumtextM1a()$y,X=as.matrix(sumtextM1a()$X)),list(betabar=Bmean,A=Bvar),MCMC),
                     "m114" = list(list(p=pMP,y=sumtextM1b()$y,X=XMPP),list(betabar=BmeanyMP,A=BvaryMP,nu=nuMP,V=VMP),MCMC),
                     "m115" = list(list(p=pMP,y=sumtextM1b()$y,X=XMPP),list(betabar=BmeanyMP,A=BvaryMP),MCMCML),
                     "m116" = list(list(y=sumtextM1a()$y,X=as.matrix(sumtextM1a()$X),k=max(sumtextM1a()$y)),list(betabar=Bmean,A=Bvar,dstarbar=Bmeancut,Ad=Bvarcut),MCMC),
                     "m117" = list(list(y=sumtextM1a()$y,X=as.matrix(sumtextM1a()$X)),list(betabar=Bmean,A=Bvar,a=a,b=b),MCMCNB),
                     "m118" = list(form=input$Formula1a, data=dataInput1(), below = Be, above = Ab, burnin = input$burnin+as.numeric(input$keep)-1, mcmc = input$it, thin=as.numeric(input$keep), verbose = 0, seed = NA, beta.start = NA, b0 = Bmean, B0 = Bvar, c0 = a, d0 = b),
                     "m119" = list(form=input$Formula1a, data=dataInput1(), tau = t, burnin = input$burnin+as.numeric(input$keep)-1, mcmc = input$it, thin=as.numeric(input$keep), verbose = 0, seed = NA, beta.start = NA, b0 = Bmean, B0 = Bvar)
      )}

    if (input$M11 == 'm111') {
      do.call(MCMCregress, args)}
    else {
      if (input$M11 == 'm112') {
        do.call(MCMClogit, args)}
      else {
        if (input$M11 == 'm113') {
          out=do.call(rbprobitGibbs, args)
          out$betadraw=as.matrix(out$betadraw)
          colnames(out$betadraw)=paste0('beta_',rownames(hot_to_r(input$hotPmean)))
          out$betadraw=as.mcmc(out$betadraw)

          out
          }
        else {
          if (input$M11 == 'm114') {
            out=do.call(rmnpGibbs, args)
            out$betadraw=as.matrix(out$betadraw)
            colnames(out$betadraw)=rownames(hot_to_r(input$hotPmeanLP))

            out$sigmadraw=as.matrix(out$sigmadraw)

            nums=NULL

            np=as.numeric(input$MultPLy)
            base=as.numeric(input$MultPLnn)
            for (i in 1:np){
              for (j in 1:np){
                if(j!=base&i!=base){
                  nums=c(nums,paste(i,j,sep = '_'))
                }

              }
            }

            colnames(out$sigmadraw)=paste0('sigma',nums)

            out$betadraw=as.mcmc(out$betadraw)
            out$sigmadraw=as.mcmc(out$sigmadraw)

            out
            }
          else {
            if (input$M11 == 'm115') {
              out=do.call(rmnlIndepMetrop, args)
              out$betadraw=as.matrix(out$betadraw)
              colnames(out$betadraw)=rownames(hot_to_r(input$hotPmeanLP))

              nums=NULL

              np=as.numeric(input$MultPLy)
              base=as.numeric(input$MultPLnn)
              for (i in 1:np){
                for (j in 1:np){
                  if(j!=base&i!=base){
                    nums=c(nums,paste(i,j,sep = '_'))
                  }

                }
              }



              out$betadraw=as.mcmc(out$betadraw)

              out
              }
            else {
              if (input$M11 == 'm116') {
                out=do.call(rordprobitGibbs, args)
                out$betadraw=as.matrix(out$betadraw)
                colnames(out$betadraw)=paste0('beta_',rownames(hot_to_r(input$hotPmean)))
                out$betadraw=as.mcmc(out$betadraw)

                out$cutdraw=as.matrix(out$cutdraw)

                cuts=paste0('cut_',1:ncol(out$cutdraw))

                out$cutdraw=as.mcmc(out$cutdraw)

                out

                }
              else {
                if (input$M11 == 'm117') {
                  out=do.call(rnegbinRw, args)
                  out$betadraw=as.matrix(out$betadraw)
                  colnames(out$betadraw)=paste0('beta_',rownames(hot_to_r(input$hotPmean)))
                  out$betadraw=as.mcmc(out$betadraw)
                  out
                  }
                else {
                  if (input$M11 == 'm118'){
                    do.call(MCMCtobit, args)}
                  else {
                    if (input$M11 == 'm119'){
                      do.call(MCMCquantreg, args)}
                    else{
                      if(input$M11=='nonpar'){
                        lm.coefs <- function(dat){
                          coef(lm(input$Formula1a, data = dat))
                        }

                        bayesboot(dataInput1(), lm.coefs, R = input$it, R2=input$burnin, use.weights = FALSE)

                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })

  ####### 1.1 Models: Download Posterior Chains##########

  output$download11 <- downloadHandler(
    filename = function() {
      paste("Posterior Chains", '.csv', sep='')
    },

    content = function(file) {

      if(input$M11=='m110')
        content<- return()

      switch(input$M11,
             "m111" = post11<- cbind(Posteriors11()),
             "m112" = post11<- cbind(Posteriors11()),
             "m113" = post11<- Draws(Posteriors11()$betadraw,input$burnin,as.numeric(input$keep)),
             "m114" = post11<- Draws(cbind(Posteriors11()$betadraw/Posteriors11()$sigmadraw[,1]^0.5,Posteriors11()$sigmadraw/Posteriors11()$sigmadraw[,1]),input$burnin,as.numeric(input$keep)),
             "m115" = post11<- Draws(Posteriors11()$betadraw,input$burnin,as.numeric(input$keep)),
             "m116" = post11<- Draws(cbind(Posteriors11()$betadraw,Posteriors11()$cutdraw),input$burnin,as.numeric(input$keep)),
             "m117" = post11<- Draws(cbind(Posteriors11()$betadraw),input$burnin,as.numeric(input$keep)),
             "m118" = post11<- cbind(Posteriors11()),
             "m119" = post11<- cbind(Posteriors11()),
            "nonpar" = post11<- cbind(Posteriors11()))
      write.csv(post11, file)
    }
  )

  ####### 1.1 Models: Summary Posterior Chains##########

  output$summary11 <- renderPrint({
    if(input$M11=='m110' || is.null(Posteriors11()) || rv_current_result$model != input$M11){
      #print(rv_current_result$model)
      #print(input$M11)
      cat('No results yet')
      }
    else{
      switch(input$M11,
             "m111" = SumDiagNormal(Posteriors11()),
             "m112" = SumDiagLogit(Posteriors11()),
             "m113" = SumDiagProbit(Posteriors11()$betadraw[,],input$it+input$burnin,input$burnin,as.numeric(input$keep)),
             "m114" = SumDiagMultProbit(Posteriors11()$betadraw[,]/Posteriors11()$sigmadraw[,1]^0.5,Posteriors11()$sigmadraw[,]/Posteriors11()$sigmadraw[,1],input$it+input$burnin,input$burnin,as.numeric(input$keep)),
             "m115" = SumDiagMultLogit(Posteriors11()$betadraw[,],input$it+input$burnin,input$burnin,as.numeric(input$keep)),
             "m116" = SumDiagOprobit(Posteriors11()$betadraw[,],Posteriors11()$cutdraw[,],input$it+input$burnin,input$burnin,as.numeric(input$keep)),
             "m117" = SumDiagNegBin(Posteriors11()$betadraw[,],input$it+input$burnin,input$burnin,as.numeric(input$keep)),
             "m118" = SumDiagTobit(Posteriors11()),
             "m119" = SumDiagQuantile(Posteriors11()),
             "nonpar" = SumDiagBayBoots(Posteriors11())
             )
    }
  })

  ####### 1.1 Models: Graphs Posterior Chains##########
  output$plot11 <- renderPlot({
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    dir.create(file.path(path,"Posterior Graphs"),showWarnings = FALSE)
    setwd(file.path(path,"Posterior Graphs"))

    graphs11<- function(post11){
      nc<-ncol(post11)
      for (i in 1:nc) {
        pdf(paste("Density Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot(post11[,i])
        dev.off()
        setEPS()
        postscript(paste("Density Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot(post11[,i])
        dev.off()
        pdf(paste("Trace Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.trace(post11[,i])
        dev.off()
        setEPS()
        postscript(paste("Trace Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.trace(post11[,i])
        dev.off()
        pdf(paste("Autocorrelation Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.corr(post11[,i])
        dev.off()
        setEPS()
        postscript(paste("Autocorrelation Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.corr(post11[,i])
        dev.off()
      }
    }


    switch(input$M11,
           "m111" = graphs11(cbind(Posteriors11())),
           "m112" = graphs11(Posteriors11()),
           "m113" = graphs11(Draws(Posteriors11()$betadraw[,],input$burnin,as.numeric(input$keep))),
           "m114" = graphs11(Draws(cbind(Posteriors11()$betadraw/Posteriors11()$sigmadraw[,1]^0.5,Posteriors11()$sigmadraw/Posteriors11()$sigmadraw[,1]),input$burnin,as.numeric(input$keep))),
           "m115" = graphs11(Draws(Posteriors11()$betadraw[,],input$burnin,as.numeric(input$keep))),
           "m116" = graphs11(Draws(cbind(Posteriors11()$betadraw[,],Posteriors11()$cutdraw[,]),input$burnin,as.numeric(input$keep))),
           "m117" = graphs11(Draws(cbind(Posteriors11()$betadraw[,]),input$burnin,as.numeric(input$keep))),
           "m118" = graphs11(Posteriors11()),
           "m119" = graphs11(Posteriors11()),
           "nonpar" = graphs11(Posteriors11())
           )
    setwd("..")
  })
  output$multiDownload11 <- downloadHandler(
    filename = function() {
      paste("Posterior Graphs", "zip", sep=".")
    },

    content = function(file) {
      zip(zipfile=file, files='Posterior Graphs')
    },
    contentType = "application/zip"
  )


  #help_file

  output$univariate_help_data=renderUI({
  text=switch(input$M11,
         "m111" = '11SimNormalmodel.csv    ',
         "m112" = '12SimLogitmodel.csv     ',
         "m113" = '13SimProbitmodel.csv     ',
         "m114" = '14SimMultProbmodel.csv     ',
         "m115" = '15SimMultLogitmodel.csv     ',
         "m116" = '16SimOrderedProbitmodel.csv     ',
         "m117" = '17SimNegBinmodel.csv     ',
         "m118" = '18SimTobitmodel.csv     ',
         "m119" = '19SimQuantilemodel.csv     ',
         "nonpar" = '41SimBootstrapmodel.csv    '
  )
  #base= 'See template file in the rstudio.cloud project, you can find it at DataSim/'
  text=paste0(base_help,text)
  helpText(text)
  })
