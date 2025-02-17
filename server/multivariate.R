

###

getRegs=function(f){
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

sim=function(DF){
  if(nrow(DF)>1){
    DF1=DF
    DF1[lower.tri(DF, diag = TRUE)]= 0
    DF=DF1+t(DF1)+diag(diag(as.matrix(DF)))
  }
  return(DF)
}

ListSUR<-function(dat,m,ki){
  st<-c(m,m+cumsum(ki))
  listreg<-NULL
  for(i in 1:m){
    listreg[[i]]=list(y=dat[,i],X=as.matrix(dat[,c((st[i]+1):st[i+1])]))
  }
  return(listreg)
}

  ######Data NavBar 2. Models #########
  dataInput2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    read.csv(inFile2$datapath, header=input$header2, sep=input$sep2)
  })


  ######Formulas NavBar 2. Models #########

  sumtextM2a <- reactive({
    model.formula(input$Formula2a,dataInput2())
  })

  sumtextM2b <- reactive({
    model.formula1(input$Formula2b,dataInput2())
  })



  ####### Output UI #####

  ##### 2.1 ########
  output$ui21 <- renderUI({
    if (input$M21=='m210'){
      return()}
    else{
      switch(input$M21,
             "m211" = isolate(wellPanel(fluidRow(column(12,HTEndVarNum)),
                                        fluidRow(column(3,EndVarNumY),column(3,ExVarNumX)),
                                        fluidRow(column(3,HTEndVarNumY),column(3,HTExVarNumX)),
                                        helpText("Prior mean matrix (k x m): Introduce prior mean location parameters. Each equation per column."),
                                        rHandsontableOutput("hotPMeanMV"),
                                        helpText("Prior covariance matrix (k x k): Introduce prior covariance matrix. It should be symmetric positive semidefinite."),
                                        rHandsontableOutput("hotPVarMV"),
                                        fluidRow(column(3,DegreeFred)),
                                        fluidRow(column(3,HTDegreeFred)),
                                        helpText("Scale matrix (m x m): Introduce prior scale matrix for inverse Wishart distribution. It should be symmetric positive semidefinite."),
                                        rHandsontableOutput("hotPVarMVWis")
   )),
   "m212" = isolate(wellPanel(fluidRow(column(12,HTEndVarNumSUR)),
                              fluidRow(column(3,EndVarNumYSUR),column(3,ExVarNumXSUR)),
                              fluidRow(column(3,HTEndVarNumYSUR),column(3,HTExVarNumXSUR)),
                              helpText("Introduce number of regressors by equation (ki). You should take into account intercepts, if you included them in your data set."),
                              rHandsontableOutput("RegEquat"),
                              helpText("Prior mean vector (1 x k): Introduce prior mean location parameters by equation."),
                              rHandsontableOutput("hotPMeanMVSUR"),
                              helpText("Prior covariance matrix (k x k): Introduce prior covariance matrix. It should be symmetric positive semidefinite."),
                              rHandsontableOutput("hotPVarMVSUR"),
                              fluidRow(column(3,DegreeFredSUR)),
                              fluidRow(column(3,HTDegreeFredSUR)),
                              helpText("Scale matrix (m x m): Introduce prior scale matrix for inverse Wishart distribution. It should be symmetric positive semidefinite."),
                              rHandsontableOutput("hotPVarMVWisSUR")

   )),
   "m213" = isolate(wellPanel(fluidRow(column(3,FormulaM2A),column(9,HTFormy)),
                              fluidRow(column(3,FormulaM2B),column(9,HTFormZ)),
                              helpText("Introduce prior mean vector location parameters (Main equation)"),
                              rHandsontableOutput("hotPmeanY"),
                              helpText("Introduce prior covariances location parameters by row. It has to be symmetric (Main equation)"),
                              rHandsontableOutput("hotPvarY"),
                              helpText("Introduce prior mean vector location parameters (Instrumental equation)"),
                              rHandsontableOutput("hotPmeanZ"),
                              helpText("Introduce prior covariances location parameters by row. It has to be symmetric (Instrumental equation)"),
                              rHandsontableOutput("hotPvarZ"),
                              helpText("Introduce scale matrix Inverse Wishart distribution"),
                              rHandsontableOutput("hotPIW"),
                              fluidRow(column(3,PshIW)),
                              fluidRow(column(3,HTshIW)))),
   "m214" = isolate(wellPanel(fluidRow(column(12,HTDataBVProbit)),
                              fluidRow(column(3,NBVProbit),column(5,ExVarNumXBVProbit),column(3,CHBVProbit)),
                              fluidRow(column(3,HTNBVProbit),column(4,HTExVarNumXBVProbit),column(3,HTCHVProbit)),
                              helpText("Introduce prior mean vector location parameters"),
                              rHandsontableOutput("hotPmeanBVProbit"),
                              helpText("Introduce prior covariances location parameters. It has to be symmetric"),
                              rHandsontableOutput("hotPvarBVProbit"),
                              fluidRow(column(3,HTMVProbit)),
                              fluidRow(column(3,NuMVProbit)),
                              rHandsontableOutput("hotPScaleBVProbit"),
                              helpText("Introduce scale matrix Inverted Wishart. It has to be symmetric")
                              ))
)
    }
  })
  ##########Multivariate regression#########
  output$hotPMeanMV=renderRHandsontable({
    if(is.null(input$hotPMeanMV)){
      nYs<-as.numeric(input$EndVarNumnY)
      nXs<-as.numeric(input$ExVarNumnX)
      DF<-data.frame(matrix(0.000,nXs,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("x",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPMeanMV)
      nYs<-as.numeric(input$EndVarNumnY)
      nXs<-as.numeric(input$ExVarNumnX)
      if(ncol(DF)!=nYs | nrow(DF)!=nXs){
        DF<-data.frame(matrix(0.000,nXs,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("x",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
  })

  output$hotPVarMV=renderRHandsontable({
    if(is.null(input$hotPVarMV)){
      nXs<-as.numeric(input$ExVarNumnX)
      DF<-data.frame(diag(100.00,nXs))
      colnames(DF)<-paste("Beta",1:nXs,sep="")
      rownames(DF)<-paste("Beta",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMV)
      nXs<-as.numeric(input$ExVarNumnX)
      if(ncol(DF)!=nXs){
        DF<-data.frame(diag(100.00,nXs))
        colnames(DF)<-paste("Beta",1:nXs,sep="")
        rownames(DF)<-paste("Beta",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
  })

  output$hotPVarMVWis=renderRHandsontable({
    if(is.null(input$hotPVarMVWis)){
      nYs<-as.numeric(input$EndVarNumnY)
      nu<-as.numeric(input$DegFredMV)
      DF<-data.frame(nu*diag(1.00,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("y",1:nYs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVWis)
      nYs<-as.numeric(input$EndVarNumnY)
      nu<-as.numeric(input$DegFredMV)
      if(ncol(DF)!=nYs){
        DF<-data.frame(nu*diag(1.00,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("y",1:nYs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red scale matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
    })

  #######SUR#########
  output$RegEquat=renderRHandsontable({
    if(is.null(input$RegEquat)){
      nYs<-as.numeric(input$EndVarNumnYSUR)
      DF<-data.frame(matrix(rep(2,input$EndVarNumnYSUR),1,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-"ki"
    }else{
      DF<-hot_to_r(input$RegEquat)
      nYs<-as.numeric(input$EndVarNumnYSUR)
      if(ncol(DF)!=nYs){
        DF<-data.frame(matrix(rep(2,input$EndVarNumnYSUR),1,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-"ki"
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    check<-sum(DF,na.rm=TRUE)!=input$ExVarNumnXSUR
    if(check){
      showNotification("Watch out! The red table means that sum of regressors by equation (ki) is not equal to TOTAL number of regressors (k)")
      rhandsontable(DF)%>%
        hot_cols(colnames(DF),format="0") %>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 td.style.background = 'red';
                 }")
    }else{
      showNotification("Good! The green table means that sum of regressors by equation (ki) is equal to TOTAL number of regressors (k). You can continue!")
      rhandsontable(DF)%>%
        hot_cols(colnames(DF),format="0") %>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 td.style.background = 'green';
                 }")
    }
    })

  output$hotPMeanMVSUR=renderRHandsontable({
    if(is.null(input$hotPMeanMVSUR)){
      ki<-rep(2,input$EndVarNumnYSUR)
      nbetas<-sum(ki)
      nYs<-as.numeric(input$EndVarNumnYSUR)
      DF<-data.frame(matrix(0,1,nbetas))
      colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
    }else{
      DF<-hot_to_r(input$hotPMeanMVSUR)
      ki<-as.numeric(hot_to_r(input$RegEquat))
      nbetas<-sum(ki)
      if(ncol(DF)!=nbetas){
        nYs<-as.numeric(input$EndVarNumnYSUR)
        ki<-as.numeric(hot_to_r(input$RegEquat))
        DF<-data.frame(matrix(0,1,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
  })

  output$hotPVarMVSUR=renderRHandsontable({
    if(is.null(input$hotPVarMVSUR)){
      ki<-rep(2,input$EndVarNumnYSUR)
      nbetas<-sum(ki)
      DF<-data.frame(diag(100.00,nbetas))
      nYs<-as.numeric(input$EndVarNumnYSUR)
      colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      rownames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVSUR)
      nbetas<-as.numeric(sum(hot_to_r(input$RegEquat)))
      nYs<-as.numeric(input$EndVarNumnYSUR)
      ki<-as.numeric(hot_to_r(input$RegEquat))
      if(ncol(DF)!=nbetas){
        DF<-data.frame(diag(100.00,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
        rownames(DF)<-paste("Beta",rep(1:nYs,times=ki),unlist(sapply(ki,function(x){rep(1:x)})),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
    })

  output$hotPVarMVWisSUR=renderRHandsontable({
    if(is.null(input$hotPVarMVWisSUR)){
      nYs<-as.numeric(input$EndVarNumnYSUR)
      nu<-as.numeric(input$DegFredMVSUR)
      DF<-data.frame(nu*diag(1.00,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("y",1:nYs,sep="")
    }else{
      DF<-hot_to_r(input$hotPVarMVWisSUR)
      nYs<-as.numeric(input$EndVarNumnYSUR)
      nu<-as.numeric(input$DegFredMVSUR)
      if(ncol(DF)!=nYs){
        DF<-data.frame(nu*diag(1.00,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("y",1:nYs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red scale matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
    })

  ###########Instrumental variable##################
  output$hotPmeanY=renderRHandsontable({

    if(is.null(input$hotPmeanY) ){
      nv = 1
      f=input$Formula2a

      DF=data.frame("Prior mean"=rep(0,nv))
      rownames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPmeanY)
      rn=rownames(DF)[-1]
      f=input$Formula2a
      if(!identical(rn,getRegs(f))){
        f=input$Formula2a
        nv=unlist(gregexpr(pattern ='~',f))

        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=rep(0,nv))
          rownames(DF)="null"
        }else{

          regs=getRegs(f)
          DF=data.frame("Prior mean"=rep(0,length(regs)+1))
          rownames(DF)=c("cte",regs)

        }

      }

    }
    DF$Prior.mean=as.numeric(DF$Prior.mean)
    rhandsontable(DF)%>%
      hot_col("Prior.mean",format="0.000")
  })

  output$hotPvarY=renderRHandsontable({

    if(is.null(input$hotPvarY) ){
      nv = 1
      f=input$Formula2a

      DF=data.frame("Prior mean"=0)
      rownames(DF)=getRegs(f)
      colnames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPvarY)
      rn=rownames(DF)[-1]
      f=input$Formula2a
      if(!identical(rn,getRegs(f))){
        f=input$Formula2a
        nv=unlist(gregexpr(pattern ='~',f))

        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{

          regs=getRegs(f)
          DF=data.frame(100*diag(length(regs)+1))
          rownames(DF)=c("cte",regs)
          colnames(DF)=c("cte",regs)

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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")

    }

    })

  output$hotPmeanZ=renderRHandsontable({

    if(is.null(input$hotPmeanZ) ){
      nv = 1
      f=input$Formula2b

      DF=data.frame("Prior mean"=rep(0,nv))
      rownames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPmeanZ)
      rn=rownames(DF)[-1]
      f=input$Formula2b
      if(!identical(rn,getRegs(f))){
        f=input$Formula2b
        nv=unlist(gregexpr(pattern ='~',f))

        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=rep(0,nv))
          rownames(DF)="null"
        }else{

          regs=getRegs(f)
          DF=data.frame("Prior mean"=rep(0,length(regs)+1))
          rownames(DF)=c("cte",regs)

        }

      }

    }
    DF$Prior.mean=as.numeric(DF$Prior.mean)
    rhandsontable(DF)%>%
      hot_col("Prior.mean",format="0.000")

  })

  output$hotPvarZ=renderRHandsontable({

    if(is.null(input$hotPvarZ) ){
      nv = 1
      f=input$Formula2b

      DF=data.frame("Prior mean"=0)
      rownames(DF)=getRegs(f)
      colnames(DF)=getRegs(f)
    }else{
      DF=hot_to_r(input$hotPvarZ)
      rn=rownames(DF)[-1]
      f=input$Formula2b
      if(!identical(rn,getRegs(f))){
        f=input$Formula2b
        nv=unlist(gregexpr(pattern ='~',f))

        if(nv==-1){
          nv = 1
          DF=data.frame("Prior mean"=0)
          rownames(DF)="null"
        }else{

          regs=getRegs(f)
          DF=data.frame(100*diag(length(regs)+1))
          rownames(DF)=c("cte",regs)
          colnames(DF)=c("cte",regs)

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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")
    }
  })


  output$hotPIW=renderRHandsontable({

    if(is.null(input$hotPIW) ){
      DF=data.frame("Main"=c(1,0),"Inst"=c(0,1))
      rownames(DF)=colnames(DF)
    }else{
      DF=hot_to_r(input$hotPIW)


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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';

                 }")
    }
  })

  #########Multivariate Probit###########

  output$hotPmeanBVProbit=renderRHandsontable({
    if(is.null(input$hotPmeanBVProbit)){
      nYs<-as.numeric(input$CHnBVProbit)
      nXs<-as.numeric(input$ExVarNumnXBVProbit)
      DF<-data.frame(matrix(0.000,nXs,nYs))
      colnames(DF)<-paste("y",1:nYs,sep="")
      rownames(DF)<-paste("x",1:nXs,sep="")
    }else{
      DF<-hot_to_r(input$hotPmeanBVProbit)
      nYs<-as.numeric(input$CHnBVProbit)
      nXs<-as.numeric(input$ExVarNumnXBVProbit)
      if(ncol(DF)!=nYs | nrow(DF)!=nXs){
        DF<-data.frame(matrix(0.000,nXs,nYs))
        colnames(DF)<-paste("y",1:nYs,sep="")
        rownames(DF)<-paste("x",1:nXs,sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
    rhandsontable(DF)%>%
      hot_cols(colnames(DF),format="0.000")
  })

  output$hotPvarBVProbit=renderRHandsontable({
    if(is.null(input$hotPvarBVProbit)){
      nYs<-as.numeric(input$CHnBVProbit)
      nbetas<-nYs*as.numeric(input$ExVarNumnXBVProbit)
      DF<-data.frame(diag(100.00,nbetas))
      ki<-as.numeric(input$ExVarNumnXBVProbit)
      colnames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
      rownames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
    }else{
      DF<-hot_to_r(input$hotPvarBVProbit)
      nYs<-as.numeric(input$CHnBVProbit)
      nbetas<-nYs*as.numeric(input$ExVarNumnXBVProbit)
      ki<-as.numeric(input$ExVarNumnXBVProbit)
      if(ncol(DF)!=nbetas){
        DF<-data.frame(diag(100.00,nbetas))
        colnames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
        rownames(DF)<-paste("Beta",rep(1:nYs,each=ki),rep(1:ki,times=nYs),sep="")
      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
    })

  output$hotPScaleBVProbit=renderRHandsontable({
    if(is.null(input$hotPScaleBVProbit)){
      l<-as.numeric(input$CHnBVProbit)
      DF<-as.numeric(input$NuMVnProbit)*data.frame(diag(1,l))
      colnames(DF)<-paste("Scale",rep(1:l),rep(1:l),sep="")
      rownames(DF)<-paste("Scale",rep(1:l),rep(1:l),sep="")
    }else{
      DF<-hot_to_r(input$hotPScaleBVProbit)
      l<-as.numeric(input$CHnBVProbit)
      if(ncol(DF)!=l){
        DF<-as.numeric(input$NuMVnProbit)*data.frame(diag(1,l))
        colnames(DF)<-paste("Scale",rep(1:l),rep(1:l),sep="")
        rownames(DF)<-paste("Scale",rep(1:l),rep(1:l),sep="")

      }
    }
    DF<-apply(DF,c(1,2),as.numeric)
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
      showNotification("Watch out! The red covariance matrix is not positive semidefinite")
      rhandsontable(DF)%>%
        hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                 td.style.background = 'red';
                 }")
    }
    })


  ######## 2.1 Models: Posterior Chains#########

  Posteriors21 <- eventReactive(input$goButton21, {
    #showNotification("Working on it. Runnig MCMC sampling", duration = 60)

    if(input$M21=='m211'){
      Bbar<- hot_to_r(input$hotPMeanMV)
      A<- solve(as.matrix(hot_to_r(input$hotPVarMV)))
      nu<- isolate(as.numeric(input$DegFredMV))
      V<- as.matrix(hot_to_r(input$hotPVarMVWis))
    }

    if(input$M21=='m212'){
      betabar<- c(hot_to_r(input$hotPMeanMVSUR))



      A<- solve(as.matrix(hot_to_r(input$hotPVarMVSUR)))
      nu<- isolate(as.numeric(input$DegFredMVSUR))
      V <- as.matrix(hot_to_r(input$hotPVarMVWisSUR))
      ki<- hot_to_r(input$RegEquat)
      nYs<-isolate(as.numeric(input$EndVarNumnYSUR))
      SURlist<-ListSUR(dataInput2(),nYs,ki)
    }

    if(input$M21=='m213'){
      Bmeany= hot_to_r(input$hotPmeanY)[,1]
      Bvary<- solve(as.matrix(hot_to_r(input$hotPvarY)))
      Bmeanz<- hot_to_r(input$hotPmeanZ)[,1]
      Bvarz<- solve(as.matrix(hot_to_r(input$hotPvarZ)))
      V<- solve(as.matrix(hot_to_r(input$hotPIW)))

      if(input$PShLIW==""){nu<-NULL}
      else{
        nu<- isolate(as.numeric(input$PShLIW))
      }
    }

    if(input$M21=='m214'){
      betabar<- c(hot_to_r(input$hotPmeanBVProbit))
      A<- solve(as.matrix(hot_to_r(input$hotPvarBVProbit)))
      nu<- isolate(as.numeric(input$NuMVnProbit))
      V<- as.matrix(hot_to_r(input$hotPScaleBVProbit))
      nxs<- isolate(as.numeric(input$ExVarNumnXBVProbit))
      nind<- isolate(as.numeric(input$NnBVProbit))
      nYs<-isolate(as.numeric(input$CHnBVProbit))
      p<- isolate(as.numeric(input$CHnBVProbit))
      Dat<- XcreateMP(p=p,nxs=nxs,nind=nind,Data=dataInput2())
      y<-NULL
      X<-NULL
      for(i in 1:dim(Dat$y)[3]){
        y<-c(y,Dat$y[,,i])
        X<-rbind(X,Dat$X[,,i])
      }
      DataMP = list(p=p, y=y, X=X)
      ind<-seq(1,p^2,p+1)
      IndB<<-rep(ind,each=nxs)
      IndS1<<-rep(ind,times=p)
      IndS2<<-rep(ind,each=p)
    }

    MCMC1<- list(R=input$itMV,keep=as.numeric(input$keepMV),burnin=input$burninMV,nprint=1000000)
    MCMC2<- list(R=input$itMV+input$burninMV,keep=1,burnin=0,nprint=1000000)
    if(input$M21=='m210'){
      return()
    }
    else{

      argsM <- switch(input$M21,
                     "m211" = list(Data=dataInput2(),m=as.numeric(input$EndVarNumnY),k=as.numeric(input$ExVarNumnX),Prior=list(Bbar=Bbar,A=A,nu=nu,V=V),MCMC1),
                     "m212" = list(Data=list(regdata=SURlist), Prior=list(betabar=betabar,A=A,nu=nu), Mcmc=MCMC2), #, Prior=list(betabar=betabar,A=A,nu=nu,V=V) It seems that there is an issue with V in rsurGibbs (see email to P. Rossi)
                     "m213" = list(list(z=as.matrix(sumtextM2b()$Z),w=as.matrix(sumtextM2a()$X[,-2]),x=sumtextM2b()$x,y=sumtextM2a()$y),list(md=Bmeanz,Ad=Bvarz,mbg=Bmeany,Abg=Bvary,nu=nu,V=V),MCMC2),
                     "m214" = list(Data=DataMP,Prior=list(betabar=betabar,A=A,nu=nu,V=V), Mcmc=MCMC2)
      )
    }
    if (input$M21 == 'm211') {


      out=do.call(MultiReg, argsM)

      out$betadraw=as.matrix(out$betadraw)
      base_cn=colnames(hot_to_r(input$hotPVarMV))
      cn=paste(base_cn,1,sep = '_')
      for (i in 2:argsM$m){
        cn=c(cn,paste(base_cn,i,sep = '_'))
      }

      beta_nums=cn

      var_names=colnames(dataInput2())[-c(1:argsM$m)][1:argsM$k]
      var_names=rep(var_names,argsM$m)
      colnames(out$betadraw)=paste(beta_nums,var_names,sep = '_')
      out$Sigmadraw=as.matrix(out$Sigmadraw)
      sigmas=c()
      n_y=sqrt(ncol(out$Sigmadraw))
      for (i in 1:n_y){
        for (j in 1:n_y){
          sigmas=c(sigmas,paste0('sigma',i,'_',j))
        }
      }
      colnames(out$Sigmadraw)=sigmas
      out$Sigmadraw=as.mcmc(out$Sigmadraw)
      out$betadraw=as.mcmc(out$betadraw)

      out


      }
    else {
      if (input$M21 == 'm212') {

        # V <<- argsM$Prior$V
        out=do.call(rsurGibbs,argsM)
        out$betadraw=as.matrix(out$betadraw)
        #colnames(out$betadraw)=colnames(hot_to_r(input$hotPMeanMVSUR))
        #print(nYs)
        var_names=colnames(dataInput2())[-c(1:nYs)]
        beta_nums=colnames(hot_to_r(input$hotPMeanMVSUR))
        var_names=var_names[1:length(beta_nums)]
        colnames(out$betadraw)=paste(beta_nums,var_names,sep = '_')
        ##sigma
        out$Sigmadraw=as.matrix(out$Sigmadraw)
        sigmas=c()
        n_y=sqrt(ncol(out$Sigmadraw))
        for (i in 1:n_y){
          for (j in 1:n_y){
            sigmas=c(sigmas,paste0('sigma',i,'_',j))
          }
        }
        colnames(out$Sigmadraw)=sigmas
        out$Sigmadraw=as.mcmc(out$Sigmadraw)
        out$betadraw=as.mcmc(out$betadraw)

        out


        }
        else {
          if (input$M21 == 'm213') {
            out=do.call(rivGibbs, argsM)

            out$deltadraw=as.matrix(out$deltadraw)
            colnames(out$deltadraw)=paste0(rownames(hot_to_r(input$hotPmeanZ)),'_delta')

            out$betadraw=as.matrix(out$betadraw)
            f=input$Formula2b
            var_prom=substr(f, 1,unlist(gregexpr(pattern ='~',f))-1) #variable con problema
            colnames(out$betadraw)=paste0(var_prom,'beta')

            out$gammadraw=as.matrix(out$gammadraw)
            beta_gamma=rownames(hot_to_r(input$hotPmeanY))
            colnames(out$gammadraw)=paste0(beta_gamma[beta_gamma!=var_prom],'_gamma')
            ### sigma
            out$Sigmadraw=as.matrix(out$Sigmadraw)
            sigmas=c()
            n_y=sqrt(ncol(out$Sigmadraw))
            for (i in 1:n_y){
              for (j in 1:n_y){
                sigmas=c(sigmas,paste0('sigma',i,'_',j))
              }
            }
            colnames(out$Sigmadraw)=sigmas
            out$Sigmadraw=as.mcmc(out$Sigmadraw)
            out$betadraw=as.mcmc(out$betadraw)
            out$gammadraw=as.mcmc(out$gammadraw)
            out$deltadraw=as.mcmc(out$deltadraw)


            out

            }
          else {
            if (input$M21 == 'm214'){
              out= do.call(rmvpGibbs, argsM)
              out$betadraw=as.matrix(out$betadraw)
              var_names=colnames(dataInput2())[-c(1,2)]
              beta_nums=colnames(hot_to_r(input$hotPvarBVProbit))
              var_names=rep(var_names,length(beta_nums)/length(var_names))
              colnames(out$betadraw)=paste(beta_nums,var_names,sep = '_')
              ####sigma

              out$sigmadraw=as.matrix(out$sigmadraw)
              sigmas=c()
              n_y=sqrt(ncol(out$sigmadraw))
              for (i in 1:n_y){
                for (j in 1:n_y){
                  sigmas=c(sigmas,paste0('sigma',i,'_',j))
                }
              }
              colnames(out$sigmadraw)=sigmas
              out$sigmadraw=as.mcmc(out$sigmadraw)
              out$betadraw=as.mcmc(out$betadraw)
              out
            }
          }
        }
      }
  })


  ####### 2.1 Models: Download Posterior Chains##########

  output$download21 <- downloadHandler(
    filename = function() {
      paste("Posterior Chains", '.csv', sep='')
    },

    content = function(file) {

      if(input$M21=='m110')
        content<- return()

      switch(input$M21,
             "m211" = post21<- cbind(Posteriors21()$bet,Posteriors21()$Sig),
             "m212" = post21<- Draws(cbind(Posteriors21()$betadraw,Posteriors21()$Sig),input$burninMV,as.numeric(input$keepMV)),
             "m213" = post21<- Draws(cbind(Posteriors21()$betadraw,Posteriors21()$deltadraw,Posteriors21()$gammadraw,Posteriors21()$Sigmadraw),input$burninMV,as.numeric(input$keepMV)),
             "m214" = post21<- Draws(cbind(Posteriors21()$betadraw/Posteriors21()$sigmadraw[,IndB]^0.5,Posteriors21()$sigmadraw[,]/(Posteriors21()$sigmadraw[,IndS1]*Posteriors21()$sigmadraw[,IndS2])^0.5),input$burninMV,as.numeric(input$keepMV)))

      write.csv(post21, file)
    }
  )


  ####### 2.1 Models: Summary Posterior Chains##########
  output$summary21 <- renderPrint({

    if(input$M21=='m210'){
      return()}

    else{
      switch(input$M21,
             "m211" = SumDiagMultiReg(Posteriors21()$bet[,],Posteriors21()$Sig[,]),
             "m212" = SumDiagSUR(Posteriors21()$betadraw,Posteriors21()$Sig[,],input$itMV+input$burninMV,input$burninMV,as.numeric(input$keepMV)),
             "m213" = SumDiagInstVar(Posteriors21()$betadraw[],Posteriors21()$deltadraw[,],Posteriors21()$gammadraw[,],Posteriors21()$Sigmadraw[,],input$itMV+input$burninMV,input$burninMV,as.numeric(input$keepMV)),
             "m214" = SumDiagBVProbit(Posteriors21()$betadraw[,]/Posteriors21()$sigmadraw[,IndB]^0.5,Posteriors21()$sigmadraw[,]/(Posteriors21()$sigmadraw[,IndS1]*Posteriors21()$sigmadraw[,IndS2])^0.5,input$itMV+input$burninMV,input$burninMV,as.numeric(input$keepMV),as.numeric(input$CHnBVProbit)))
    }
  })


  ####### 2.1 Models: Summary Posterior Chains##########

  output$plot21 <- renderPlot({
    unlink(file.path(path,"Posterior Graphs"),recursive=TRUE)
    dir.create(file.path(path,"Posterior Graphs"),showWarnings = FALSE)
    setwd(file.path(path,"Posterior Graphs"))

    graphs21<- function(post21){
      nc<-ncol(post21)
      for (i in 1:nc) {
        pdf(paste("Density Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Density Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot(post21[,i])
        dev.off()
        pdf(paste("Trace Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.trace(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Trace Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.trace(post21[,i])
        dev.off()
        pdf(paste("Autocorrelation Plot",paste(i,".pdf", sep = "", collapse = NULL)))
        Plot.corr(post21[,i])
        dev.off()
        setEPS()
        postscript(paste("Autocorrelation Plot",paste(i,".eps", sep = "", collapse = NULL)))
        Plot.corr(post21[,i])
        dev.off()
      }
    }
    switch(input$M21,
           "m211" = graphs21(cbind(Posteriors21()$bet[,],Posteriors21()$Sig[,])),
           "m212" = graphs21(Draws(cbind(Posteriors21()$bet,Posteriors21()$Sig),input$burninMV,as.numeric(input$keepMV))),
           "m213" = graphs21(Draws(cbind(Posteriors21()$betadraw[],Posteriors21()$deltadraw[,],Posteriors21()$gammadraw[,],Posteriors21()$Sigmadraw[,]),input$burninMV,as.numeric(input$keepMV))),
           "m214" = graphs21(Draws(cbind(Posteriors21()$betadraw[,]/Posteriors21()$sigmadraw[,IndB]^0.5,Posteriors21()$sigmadraw[,]/(Posteriors21()$sigmadraw[,IndS1]*Posteriors21()$sigmadraw[,IndS2])^0.5),input$burninMV,as.numeric(input$keepMV))))
    setwd("..")
  })

  output$multiDownload21 <- downloadHandler(
    filename = function() {
      paste("Posterior Graphs", "zip", sep=".")
    },

    content = function(file) {
      zip(zipfile=file, files='Posterior Graphs')
    },
    contentType = "application/zip"
  )





  output$multivariate_help_data=renderUI({
    text=switch(input$M21,
                "m211" = '21SimMultivariate.csv    ',
                "m212" = '22SimSUR.csv     ',
                "m213" = '23SimIV.csv     ',
                "m214" = '24SimMultProbit.csv     '

    )
    #base= 'See template file in the rstudio.cloud project, you can find it at DataSim/'
    text=paste0(base_help,text)
    helpText(text)
  })


