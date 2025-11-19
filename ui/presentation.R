
tabPanel("Presentation",
         sidebarLayout(
           sidebarPanel(h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")),
                        h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
                        image,
                        h4("Professor Andres Ramirez Hassan"),
                        h4("Mateo Graciano Londono"),
                        h4(span("aramir21@gmail.com" , style = "color:blue"))),
           mainPanel(h3(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")," is a team of friends from ", a("Universidad EAFIT", href ="http://www.eafit.edu.co/Paginas/index.aspx"), " (Medellin, Colombia) that promoves research, teaching and encoding of Bayesian Econometrics with social responsibility."
           ),
           h3("Bayesian Econometrics allows establishing a framework that simultaneously unifies decision theory, statistical inference, and probability theory under a single philosophically and mathematically consistent structure."),
           br(),
           h3(strong("VISION")),
           h4(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian"), " envisions a global econometric community in which research, teaching, and applied work are grounded in a Bayesian framework that:"),
           h4(em("inspires"), " new econometric ideas,"),
           h4(em("creates"), " a user-friendly environment for Bayesian econometric applications, and"),
           h4(em("transforms"), " classical approaches to econometric research, teaching, and practice."),
           br(),
           h3(strong("MISSION")),
           h4(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian"), "leads and excels in the generation and dissemination of Bayesian econometric knowledge through research, teaching and software.")
           )),
  uiOutput("presentation")
)
