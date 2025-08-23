tabPanel("Bayesian Model Averaging",
         sidebarLayout(
           sidebarPanel(
             radioBMA,
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image
           ),
           mainPanel(CONDBMA)
         )
)
