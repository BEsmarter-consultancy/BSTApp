



build_formula <- function(df,model){
  if (model %in% c('m114','m115')){

    opt <- c('Dependent','Alt Specific','Not Alt Specific','None')

    ys <- rownames(df)[df$Type=='Dependent']

    if (length(ys)!=1){
      return('error, there must be (exactly) 1 Dependent variable')
    }

    xs1 <- rownames(df)[df$Type=='Alt Specific']

    xs2 <- rownames(df)[df$Type=='Not Alt Specific']

    xs <- c(xs1,xs2)

    if (length(xs)==0){
      return('error, there must at least 1 regressor')
    }

    rs <- paste(xs, collapse = '+')

    form <- paste(ys,'~',rs)

    return(form)

  }else{#'Dependent',
    ys <- rownames(df)[df$Type=='Dependent']
    if (length(ys)!=1){
      return('error, there must be (exactly) 1 Dependent variable')
    }

    xs <- rownames(df)[df$Type=='Independent']

    if (length(xs)==0){
      return('error, there must at least 1 Independent variable')
    }

    rs <- paste(xs, collapse = '+')


    form <- paste(ys,'~',rs)

    if(model=='m116'){
      form <- paste0(form,'-1')
    }

    return(form)

  }


}

aux_formula_ui <- function(id,data=NULL) {
  print('hello')
  if(is.null(data)){

  }else{
    fluidPage(h3('Data preview'),tableOutput(NS(id,'preview_table')),
              h3('Formula builder'),
               rHandsontableOutput(NS(id,'formula_table')),
              br(),

              br()
    )
  }

}




aux_formula_server <- function(id,model,data) {

  stopifnot(is.reactive(data))



  moduleServer(id, function(input, output, session) {

    rv_rerun = reactiveValues(updated_table=0)
    output$preview_table=renderTable({


      DF <- head(data())
      rv_rerun$updated_table=0
      #print(rv_rerun$updated_table)

      DF

    })




    output$formula_table=renderRHandsontable({



      opt <- c('Dependent','Independent','None')

      if (model() %in% c('m114','m115')){
        opt <- c('Dependent','Alt Specific','Not Alt Specific','None')
      }

      if(is.null(input$formula_table)|| rv_rerun$updated_table==0 ){

        rv_rerun$updated_table= 1
        print(rv_rerun$updated_table)
        DF <- t(data()[1,])

        DF <- data.frame(DF)

        colnames(DF) = 'Type'

        DF[,1] <- rep('None',nrow(DF))

        DF[,1] <- factor(DF[,1],levels = opt)

        #DF <- t(DF)

      }else{



        DF=hot_to_r(input$formula_table)


        if(length(setdiff(rownames(DF),colnames(data())))>0){
          DF <- t(data()[1,])

          DF <- data.frame(DF)

          colnames(DF) = 'Type'

          DF[,1] <- rep('None',nrow(DF))

          DF[,1] <- factor(DF[,1],levels = opt)
        }
      }


      rhandsontable(DF)%>%
        hot_col(col = colnames(DF), type = "dropdown", source = opt)%>%
        hot_cols(colWidths = rep(100,ncol(DF)))%>%
        hot_rows(rowHeights = 50, fixedRowsTop = 1)


    })

    reactive({
      hot_to_r(input$formula_table)
    })



  })
}








aux_preview_ui <- function(id,data=NULL) {

  if(is.null(data)){

  }else{
    fluidPage(h3('Data preview'),tableOutput(NS(id,'preview_table')),
              br(),
              br()
    )
  }

}




aux_preview_server <- function(id,data) {

  stopifnot(is.reactive(data))



  moduleServer(id, function(input, output, session) {


    output$preview_table=renderTable({


      DF <- head(data())
      DF

    })
  })
}





