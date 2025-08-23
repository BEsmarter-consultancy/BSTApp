tabPanel("Help",
         sidebarLayout(
           sidebarPanel(
             h1(a(em(strong("BEsmarter",style = "color:light blue")),href = "https://sites.google.com/view/arh-bayesian")),
             h2("Bayesian Econometrics: simulations, models and applications to research, teaching and encoding with responsibility"),
             image,
             h4("Professor Andres Ramirez Hassan"),
             h4("Mateo Graciano Londono"),
             h4(span("aramir21@gmail.com", style = "color:blue"))),
           mainPanel(
           h4(a(em(strong('Ramirez-Hassan, A. & Graciano-Londono, M. (2019). "A GUIded tour of Bayesian regression"',style = 'color:light blue')),href = "https://bookdown.org/aramir21/IntroductionBayesianEconometricsGuidedTour")
           ),
           tags$iframe(style="height:600px; width:400px", src="https://drive.google.com/file/d/1_IBKe7vS5a2XLnvg74T_UNESRoKNDUUt/view?usp=sharing")
           #tags$iframe(style="height:600px; width:100%", src="docs/help.pdf")
           #tags$iframe(style="height:600px; width:100%", src="http://www.besmarter-team.org/files/working_papers/A%20GUIded%20tour%20of%20Bayesian%20regression.pdf")
           )
         ))
