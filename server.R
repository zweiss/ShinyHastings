library(shiny)
library(shinyjs)
library(ggmcmc)
source("helper.R")

################################################################################################################################
# This code was written by Zarah Weiss
# It contains the server logic of ShinyHastings
################################################################################################################################


# Define server logic for MH algorithm with varying target types
shinyServer(function(input, output, session) {
  
  ##################################### I. Run program
  
  target <- reactive({
    target <- switch (input$targetChoice,
                      normal = rnorm(input$targetObs, mean = input$targetMean, sd = input$targetSD),
                      log_normal = rlnorm(input$targetObs, mean = input$targetMean, sd = input$targetSD), 
                      rnorm)
  })

  settingText <- eventReactive(input$approximationButton, {
    
    proposal <- switch (input$proposalType,
                        random_walk = "Simple Uniform Random Walk",
                        random_walk.normal = "Simple Normal Random Walk",
                        mala = "Metropolis-Aligned Langevin",
                        independence = "Independence Normal Metropolis",
                        independence.t = "Independence Student Metropolis")
    tuning <- switch (input$proposalType,
                      random_walk = paste0("Gamma: +/- ", input$epsilon),
                      random_walk.normal = paste0("Omega: ", input$omega),
                      mala = paste0("Gamma: +/- ", input$epsilon, ", Gradient: ", input$gradient),
                      independence = "No tuning parameter",
                      independence.t = paste0("Degrees of freedom: ", input$degrees_of_freedom))
    target_distr <- switch (input$targetChoice,
                            normal = paste0("Normal distribution"),
                            log_normal = paste0("Log Normal distribution"))
    
    str <- paste0("1. Algorithm settings:\n\ta. Proposal type: ", proposal, "\n\tb. ", tuning, "\n\n",
                  "2. Experiment settings:\n\ta. Target distribution: ", target_distr, " with ", input$targetObs, " observations",
                                         "\n\tb. Mean: ", input$targetMean, "\t\t\tStandard deviation: ", input$targetSD,
                                         "\n\tc. Initial mean: [", input$proposalMeanMin, ", ", input$proposalMeanMax, "]\tInitial standard deviation: [", input$proposalSDMin, ", ", input$proposalSDMax, "]\n\n",
                  "3. Iteration set-up:\n\ta. Chains: ", input$numberOfChains, 
                                      "\n\tb. Iterations: ", input$numberOfIterationsAndBurnIn[2],
                                      "\n\tc. Burn-in: ", input$numberOfIterationsAndBurnIn[1], "\n")
      
    return(str)
  })
  
  mh_out <- eventReactive(input$approximationButton, {
    
    # disable action button
    disable("approximationButton")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Approximating target", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    tuning <- switch (input$proposalType,
                      random_walk = input$epsilon,
                      random_walk.normal = c(input$omega, input$targetObs),
                      mala = c(input$epsilon, input$gradient),
                      independence = c(input$proposalMeanMin, input$proposalMeanMax, input$proposalSDMin, input$proposalSDMax),
                      independence.t = c(input$degrees_of_freedom))
    
    # try to run mh and enable approximation button (even if error occurred)
    tryCatch(
      out <- mh.wrapper(data = target(),
                        proposal_distr = input$targetChoice,
                        proposal_mean = c(input$proposalMeanMin, input$proposalMeanMax),
                        proposal_sd = c(input$proposalSDMin, input$proposalSDMax),
                        proposal_type = input$proposalType,
                        tuning = tuning,
                        iterations = input$numberOfIterationsAndBurnIn[2],
                        burnIn = input$numberOfIterationsAndBurnIn[1],
                        # iterations = input$numberOfIterations,
                        # burnIn = input$burnInSize,
                        chains = input$numberOfChains,
                        updateProgress
      ),          
      error = function(e) return(),
      finally = enable("approximationButton")
    )
  })
  
  
  
  #################################### II. Display results

  output$summary <- renderPrint({
    if(input$approximationButton==0) return()
    summary(mh_out())
  })
  
  output$trace <- renderPlot({
    if(input$approximationButton==0) return()
    ggs_traceplot(ggs(mh_out()))
  })
  
  output$density <- renderPlot({
    if(input$approximationButton==0) return()
    ggs_density(ggs(mh_out()))
  })
  
  output$hdi <- renderPrint({
    if(input$approximationButton==0) return()
    ggs(mh_out()) %>% group_by(Parameter) %>% 
      summarise(mean = mean(value),
                HDIlow  = HPDinterval(as.mcmc(value))[1],
                HDIhigh = HPDinterval(as.mcmc(value))[2])
  })
  
  output$auto <- renderPlot({
    if(input$approximationButton==0) return()
    ggs_autocorrelation(ggs(mh_out()))
  })
  
  output$rhat <- renderPrint({
    if(input$approximationButton==0) return()
    gelman.diag(mh_out())
  })
  
  output$effective_size <- renderPrint({
    if(input$approximationButton==0) return()
    effectiveSize(mh_out())
  })
  
  output$acceptance <- renderPrint({
    if(input$approximationButton==0) return()
    return(1 - rejectionRate(mh_out()))
  })
  
  output$target_choice <- renderPlot({
    hist(target(), breaks = 10)
  })
  
  output$settings <- renderPrint({
    if(input$approximationButton==0) return()
    cat(settingText())
  })
  
  
})