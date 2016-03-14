library(shiny)
library(shinyjs)

################################################################################################################################
# This code was written by Zarah Weiss
# It contains the user interface of ShinyHastings
################################################################################################################################


# Define UI 
shinyUI(fluidPage(
  # load shinyjs for disable/enable buttons
  useShinyjs(),
  
  
  ##########################################################################################################################################
  #     Title
  ##########################################################################################################################################
  
  titlePanel("ShinyHastings: On the Proposal Types' Effect on the Metropolis Hastings Algorithm"),
  br(),
  
  sidebarLayout(
    
    ##########################################################################################################################################
    #     Sidebar: Settings
    ##########################################################################################################################################
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  
                  
                  ##########################################################################################################################################
                  #     Sidebar: Settings: Algorithm tab
                  ##########################################################################################################################################
                  
                  tabPanel("Algorithm",
                           h3("Algorithm Settings"),
                           # choose proposal type
                           selectInput("proposalType", 
                                       label = h5("Proposal type:"),
                                       choices = c("Simple Uniform Random Walk" = "random_walk", 
                                                   "Simple Normal Random Walk" = "random_walk.normal", 
                                                   "Metropolis-Aligned Langevin" = "mala", 
                                                   "Independence Normal Metropolis" = "independence",
                                                   "Independence Student Metropolis" = "independence.t"),
                                       selected = "random_walk"),
                           # add epsilon / omega for suited proposal types
                           conditionalPanel(
                             condition = "input.proposalType == 'random_walk'",
                             numericInput("epsilon", label = "Choose gamma", value = .2, step = .01, min = 1e-100)
                           ),
                           conditionalPanel(
                             condition = "input.proposalType == 'random_walk.normal'",
                             numericInput("omega", label = "Choose omega", value = .1, step = .01, min = 1e-100) 
                           ),
                           # add gradient for suited proposal types
                           conditionalPanel(
                             condition = "input.proposalType == 'mala'",
                             column(6, numericInput("epsilon", label = "Choose gamma", value = .2, step = .01, min = 1e-100)),
                             column(6, numericInput("gradient", label = "Choose gradient", value = .1, step = .01, min = 1e-100, max = 1))
                           ),
                           conditionalPanel(
                             condition = "input.proposalType == 'independence.t'",
                             numericInput("degrees_of_freedom", label = "Choose degrees of freedom", value = 10, step = 1, min = 1) 
                           ),
                           # 4. action button
                           br(),
                           actionButton("approximationButton", "Approximate target!"),
                           br(),
                           br(),
                           h5("Please note:"),
                           helpText(p("When opened for the first time, the program displays NULL as it waits for the 
                                       'Approximate target!' button to be clicked, before executing."),
                                    p(strong("Decide on your settings and click 'Approximate target!' to see the results!")))
                  ),
                  
                  ##########################################################################################################################################
                  #     Sidebar: Settings: Experiment tab
                  ##########################################################################################################################################
                  
                  tabPanel("Experiment",
                           # choose target and proposal distribution
                           h3("Experiment Settings"),
                           selectInput("targetChoice", label = h5("Target distribution:"),
                                       choices = list("Normal" = "normal", 
                                                      "Log normal" = "log_normal"#,
                                                      # "Bimodal" = "bimodal"
                                       ), 
                                       selected = "normal"),
                           checkboxGroupInput("adjust", label = h5("Advanced settings"), 
                                              choices = list("Target distribution" = "target", "Initial parameters" = "proposal")),
                           fluidRow(
                             # adjust target distribution
                             conditionalPanel(
                               condition = "input.adjust == 'target' | input.adjust.length == 2",
                               column(4, numericInput("targetObs", label = h5("Target obs."), value = 200, min = 2, step = 1)),
                               column(4, numericInput("targetMean", label = h5("Target mean"), value = 0, step = 1)),
                               column(4, numericInput("targetSD", label = h5("Target sd"), value = 1, min = 1e-100))
                             ),
                             # adjust proposal distribution
                             conditionalPanel(
                               condition = "(input.adjust == 'proposal' | input.adjust.length == 2)",
                               column(3, numericInput("proposalMeanMin", label = h5("Mean min"), value = -2, step = 1)),
                               column(3, numericInput("proposalMeanMax", label = h5("Mean max"), value = 2, step = 1)),
                               column(3, numericInput("proposalSDMin", label = h5("SD min"), value = 1e-3, min = 1e-100)),
                               column(3, numericInput("proposalSDMax", label = h5("SD max"), value = 1, min = 1e-100))
                             )
                           ),
                           # 3. decide on how many chains, iterations, burn-ins
                           h3("Iteration Set-up"),
                           numericInput("numberOfChains",  label = h5("Chains"), value = 2, min = 1),
                           sliderInput("numberOfIterationsAndBurnIn", label = h5("Burn-in size (min) and number of iterations (max)"), 
                                       value = c(50000,100000), min = 1000, max = 500000, step = 1000)
                  ),
                  
                  ##########################################################################################################################################
                  #     Sidebar: Settings: Instruction tab
                  ##########################################################################################################################################
                  
                  tabPanel("Instructions",
                           h3("Instructions"),
                           helpText(
                             p("This app was created to show the influence of proposal types on the Metropolis Hastings algorithm.
                               In the settings panel under the algorithm tab, you can choose between different proposal types and adjust 
                               their parameters. Click 'Aproximate target!' to start the algorithm. Caution: this will take a while 
                               and the button will be disabled, until the sampling process has finished!"),
                             p("In the experiment settings tab, you may also adjust:"),
                             p("1) the target distribution to approximate and the initial parameters for the algorithm,"),
                             p("2) the iteration set-up."),
                             p("Note: More iterations and chains increase sampling accuracy as well as sampling duration."),
                             p("For further instructions and implementational details, please consult the ",
                               a("Github project page", href = "https://github.com/zweiss/ShinyHastings"),
                               "!")
                           )
                  )
      )),
    
    ##########################################################################################################################################
    #     Main panel: MH results
    ##########################################################################################################################################
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("HDI", verbatimTextOutput("hdi")),
                  tabPanel("Auto-correlation", plotOutput("auto")),
                  tabPanel("Effective sample size", verbatimTextOutput("effective_size")),
                  tabPanel("R-hat", verbatimTextOutput("rhat")),
                  tabPanel("Trace", plotOutput("trace")),
                  tabPanel("Density", plotOutput("density")),
                  tabPanel("Acceptance rate", verbatimTextOutput("acceptance")),                  
                  tabPanel("Distribution choice", plotOutput("target_choice")),                  
                  tabPanel("Used settings", verbatimTextOutput("settings"))
      )
    )
  )
))