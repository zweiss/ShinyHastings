library(shiny)
library(shinyjs)

# Define UI 
shinyUI(fluidPage(
  # load shinyjs for disable/enable buttons
  useShinyjs(),
  
  
  ##########################################################################################################################################
  #     Title
  ##########################################################################################################################################
  
  titlePanel("Shiny Hastings: On the Proposal Density's Effect on Efficiency"),
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
                                       choices = c("Uniform random-walk metropolis" = "random_walk", 
                                                   "Normal random-walk metropolis" = "random_walk.normal", 
                                                   "Metropolis-adjusted Langevin" = "mala", 
                                                   "Independence metropolis" = "independence", 
                                                   "Choice 3" = 3), 
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
                           # 4. action button
                           br(),
                           actionButton("approximationButton", "Approximate target!"),
                           br(),
                           br(),
                           h5("Please note:")#,
#                             helpText(p("When opened for the first time, the program displays NULL as it waits for the, 
#                                        'Approximate target!' button to be clicked, before executing."),
#                                      p(strong("Decide on your settings and click 'Approximate target!' to see the results!")))
                 ),
   
   ##########################################################################################################################################
   #     Sidebar: Settings: Experiment tab
   ##########################################################################################################################################
   
               tabPanel("Experiment",
                           # choose target and proposal distribution
                           h3("Experiment Settings"),
#                            selectInput("targetChoice", label = h5("Target distribution:"),
#                                      choices = list("Normal" = "normal", 
#                                                     "Log normal" = "log_normal",
#                                                     "Bimodal" = "bimodal"
#                                      ), 
#                                      selected = "normal"),
                           fluidRow(
                             column(6, radioButtons("targetChoice", label = h5("Target distribution:"),
                                                    choices = list("Normal" = "normal", 
                                                                   "Log normal" = "log_normal",
                                                                   "Bimodal" = "bimodal"), 
                                                    selected = "normal")),
                             column(6, radioButtons("proposalChoice", label = h5("Proposal distribution:"),
                                                    choices = list("Normal" = "normal",
                                                                   "Log normal" = "log_normal",
                                                                   "Bimodal" = "bimodal"), 
                                                    selected = "normal"))
                           ),
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
#                            fluidRow(
#                              column(4,
#                                     numericInput("numberOfChains",  label = h5("Chains"), value = 3, min = 1)
#                              ),
#                              column(4,
#                                                  numericInput("numberOfIterations", label = h5("Iterations"), value = 10000, min = 1)
#                              ),
#                              column(4,
#                                     sliderInput("burnInSize", label = "Size of burn-in phase", value = 5000, min = 0, max = 500000, step = 500)
#                                     numericInput("burnInSize", label = h5("Burn-in"), value = 5000, min = 0)
#                              )
#                            )
                          numericInput("numberOfChains",  label = h5("Chains"), value = 2, min = 1),
                          sliderInput("numberOfIterationsAndBurnIn", label = h5("Burn-in size (min) and number of iterations (max)"), 
                                      value = c(50000,100000), min = 1000, max = 500000, step = 1000)
                  ),

  ##########################################################################################################################################
  #     Sidebar: Settings: Instruction tab
  ##########################################################################################################################################

                  tabPanel("Instructions",
                           h3("Instructions")#,
#                            helpText(
#                              p("This app was created to show the influence of proposal types on the Metropolis Hastings algorithm.
#                                In the 'main' tab, you can choose between different proposal types and adjust their parameters. Click 'Aproximate!'
#                                to start the algorithm. Caution: this will take a while!"),
#                              
#                              p("Please note: When opened for the first time, the program will run for a while to perform the first analysis,
#                                before you can make your own adjustments!"),
#                              p("In the 'settings' tab, you may also adjust:"),
#                              p("1) The target distribution to approximate and the sampling distributions to approximate with.
#                                Note: in order to achieve reasonable results, they should be similar."),
#                              p("2) The iteration set-up. Note: more iterations and chains increase sampling accuracy as well as sampling duration.
#                                Also, the number of burn-in iterations cannot exceed the number of iterations. If it does, the program stops."),
#                              p("For further instructions and implementational details, please consult the ",
#                                a("Github project page", href = "https://github.com/zweiss/ShinyHastings"),
#                                "!")
#                              )
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
                  tabPanel("Distribution choice", fluidRow(
                    column(3, plotOutput("target_choice")),
                    column(3, plotOutput("proposal_choice"))
                  ))
      )
    )
  )
))