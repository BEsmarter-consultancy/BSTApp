tabPanel("Multivariate Models",
         sidebarLayout(
           sidebarPanel(
             radioButtons("M21", "Models",
                          c("No Selection"="m210","Simple Multivariate"="m211", "Seemingly Unrelated Regression"="m212","Instrumental Variable (Two Equations)"="m213","Multivariate Probit"="m214")
             ),
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(fluidRow(column(5,file2m),column(2,filech2m),column(2,rb2m)),
                     uiOutput("multivariate_help_data"),
                     fluidRow(column(4,it1MV),column(4,it2MV),column(4,it3MV)),DUI21,HT,go21,BE,DL21,DLP21,verbatimTextOutput("summary21"),pplot21)),

  uiOutput("multivariate")
)
