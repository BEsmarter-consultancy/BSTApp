
tabPanel("Hierarchical Longitudinal Models",
         sidebarLayout(
           sidebarPanel(
             radioButtons("M31", "Models",
                          c("No Selection"="m310","Normal"="m311", "Logit"="m312","Poisson"="m313")
             ),
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(fluidRow(column(5,file3m),column(2,filech3m),column(2,rb3m)),
                     uiOutput("hierarchical_help_data"),
                     fluidRow(column(4,it1HM),column(4,it2HM),column(4,it3HM)),DUI31,HT,go31,BE,DL31,DLP31,verbatimTextOutput("summary31"),pplot31)),

  uiOutput("hierarchical")
)
