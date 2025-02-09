tabPanel("Time Series Models",
         sidebarLayout(
           sidebarPanel(
             radioButtons("Mts", "Models",
                          c("No Selection"="0",
                            "Dynamic Linear Models"="DML",
                            "ARMA"="ARMA",
                            "Stochastic volatility Models"="SVM",
                            "VAR"="VAR")
             ),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(fluidRow(column(5,file6m),column(2,filech6m),column(2,rb6m)),
                     uiOutput("time_seriesHT"),
                     it_row_ts,
                     uiOutput("time_seriesUI"))
)
)
