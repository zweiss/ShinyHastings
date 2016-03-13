require('coda')

################################################################################################################################
# This code was written by Zarah Weiss
# It contains the MH algorihm for ShinyHastings
################################################################################################################################


##########################################################################################################################################
#     Posterior, prior & likelihood
##########################################################################################################################################

# implements unnormalized log posterior with mu and sigma
posterior_log <- function(data, distribution, mu, muMin, muMax, sigma, sigmaMin, sigmaMax) {
  if (sigma <= 0 ){
    return(0)
  }
  return(prior_log(mu, muMin, muMax, sigma, sigmaMin, sigmaMax) + likelihood_log(data, distribution, mu, sigma))
}

# log prior of sigma and mu
prior_log <- function(mu, muMin, muMax, sigma, sigmaMin, sigmaMax) {
  return(dunif(mu, min = muMin, max = muMax, log = T) + dunif(sigma, min = sigmaMin, max = sigmaMax, log = T))
}

# log likelihood for some data, given some distributionibution using sigma and mu 
likelihood_log <- function(data, distribution, mu, sigma) {
  return(sum(distribution(data, mean = mu, sd = sigma, log = T)))
}


##########################################################################################################################################
#     Proposal distributionibutions 
##########################################################################################################################################

random_walk_proposal <- function(x, proposal_params) {
  gamma = proposal_params[1]
  return(x + runif(1, min = -gamma, max = gamma))
}

random_walk_proposal_normal <- function(x, proposal_params) {
  omega = proposal_params[1]
  return(x + rnorm(1, mean = 0, sd = omega))
}

mala_proposal <- function(x, posterior_x, proposal_params) {
  gamma = proposal_params[1]
  gradient = proposal_params[2]
  return(x + .5 * gradient^2 * posterior_x + gradient * runif(1, min = -gamma, max = gamma))
}

independence_normal_proposal <- function(proposal_params) {
  muMin = proposal_params[1]
  muMax = proposal_params[2]
  sigmaMin = proposal_params[3]
  sigmaMax = proposal_params[4]
  mu = runif(1, min = muMin, max = muMax)
  sigma = runif(1, min = sigmaMin, max = sigmaMax)
  return(rnorm(1, mean = mu, sd = sigma))
}

independence_tstudent_proposal <- function(proposal_params) {
  degrees_of_freedom = proposal_params[1]
  return(rt(1, degrees_of_freedom))
}


##########################################################################################################################################
#     Acceptance probabilties                                               
##########################################################################################################################################

random_walk_acceptance_log <- function(x, y, acceptance_params) {
  return(random_walk_proposal(x, acceptance_params) + random_walk_proposal(y, acceptance_params))
}

random_walk_acceptance_normal_log <- function(x, y, acceptance_params) {
  return(random_walk_proposal_normal(x, acceptance_params) + random_walk_proposal_normal(y, acceptance_params))
}

mala_acceptance_log <- function(x, y, yPosterior, acceptance_params) {
  gradient = acceptance_params[2]
  par = (x - y) / (gradient - gradient * yPosterior / 2)
  return(mala_proposal(par, yPosterior, acceptance_params)) 
}

independence_normal_acceptance_log <- function(acceptance_params) {
  # just ignore x
  return(independence_normal_proposal(acceptance_params))
}

independence_tstudent_acceptance_log <- function(acceptance_params) {
  # just ignore x
  return(independence_tstudent_proposal(acceptance_params))
}

##########################################################################################################################################
#     Metropolis Hastings algorithm: Wrapper                                                                                                      
##########################################################################################################################################

# calls different implementations of MH algorithm according to parameters
mh.wrapper <- function(data, proposal_distribution, proposal_mean, proposal_sd, proposal_type, tuning, iterations, chains, burnIn,
                       updateProgress = NULL) {
  
  # number of burn-ins cannot exceed number of iterations
  if(burnIn > iterations) {
    return(0)
  }
  
  # choose proposal distributionibution
  distribution = switch (proposal_distribution,
                  normal = dnorm,
                  log_normal = dlnorm,
                  bimodal = dbimodal,
                  dnorm
  )
  
  # choose proposal type
    if(proposal_type == "random_walk") {
      proposal <- random_walk_proposal
      acceptance <- random_walk_acceptance_log
      random <- 1
      print("random uniform")
    } else if (proposal_type == "random_walk.normal") {
      proposal <- random_walk_proposal_normal
      acceptance <- random_walk_acceptance_normal_log
      random <- 1
      print("random normal")
    } else if (proposal_type == "mala") {
      proposal <- mala_proposal
      acceptance <- mala_acceptance_log
      random <- 0
      print("mala")
    } else if (proposal_type == "independence") {
      proposal <- independence_normal_proposal
      acceptance <- independence_normal_acceptance_log
      random <- -1 
      print("independence normal")
    } else if (proposal_type == "independence.t") {
      proposal <- independence_tstudent_proposal
      acceptance <- independence_tstudent_acceptance_log
      random <- -1
      print("independence t")
    } else {
      # default, just in case...
      proposal <- random_walk_proposal
      acceptance <- random_walk_acceptance_log
      random <- 1
      print("default normal")
    }
  
  return(metropolis_hastings(data, 
                             distribution, proposal, acceptance, 
                             proposal_mean, proposal_sd, tuning, 
                             random, 
                             chains, iterations, burnIn,
                             updateProgress = NULL))
}


##########################################################################################################################################
#     Metropolis Hastings algorithm: Actual MH implementation                                                                                                      
##########################################################################################################################################

metropolis_hastings <- function(data, 
                                distribution, proposal, acceptance, 
                                proposal_mean, proposal_sd, tuning, 
                                type, 
                                chains, iterations, burnIn,
                                updateProgress = NULL) {
  
  # initialize output array for results for each chain and iteration of parameters \mu and \sigma
  out = array(0, dim = c(chains, iterations - burnIn, 2))
  dimnames(out) = list("chain" = 1:chains, "iteration" = 1:(iterations-burnIn), "variable" = c("mu", "sigma"))
  
  # set-up min and max for mu and sigma
  muMin <- proposal_mean[1]
  muMax <- proposal_mean[2]
  sigmaMin <- proposal_sd[1]
  sigmaMax <- proposal_sd[2]
  
  num_accept = 0
  
  # for each chain
  for (c in 1:chains) {
    
    print(c)
    
    # I. Initialization phase
    
    # start with uniformly random sample over the parameter's support
    mu = runif(1, min = muMin, max = muMax)
    sigma = runif(1, min = sigmaMin, max = sigmaMax)
    # calculate the posterior for this state
    posteriorCurrentState = posterior_log(data, distribution, mu, muMin, muMax, sigma, sigmaMin, sigmaMax)
    
    # for each iteration in a chain
    for (i in 1:iterations) {
      
      # II. Proposal phase
      
      # propose new state
      # types are admittedly an annoying and random way to ensure correct parameters, 
      # but still better than separate MH algorithm functions for each one
      if(type == 1) { 
        muNext = proposal(mu, tuning)
        sigmaNext = proposal(sigma, tuning)
      } else if (type == 0) {
        muNext = proposal(mu, exp(posteriorCurrentState), tuning) 
        sigmaNext = proposal(sigma, exp(posteriorCurrentState), tuning)
      } else {
        muNext = proposal(tuning)
        sigmaNext = proposal(tuning)
      } 
      posteriorNextState = posterior_log(data, distribution, muNext, muMin, muMax, sigmaNext, sigmaMin, sigmaMax)
      
      ## III. Decision phase
      
      # transition probabilities
      if(type == 1) {
        transition_ratio_log = acceptance(mu, muNext, tuning) + acceptance(sigma, sigmaNext, tuning) 
                              - acceptance(muNext, mu, tuning) + acceptance(sigmaNext, sigma, tuning)
      } else if (type == 0){
        transition_ratio_log = acceptance(mu, muNext, posteriorNextState, tuning) + acceptance(sigma, sigmaNext, posteriorNextState, tuning) 
                              - acceptance(muNext, mu, posteriorCurrentState, tuning) + acceptance(sigmaNext, sigma, posteriorCurrentState, tuning)
      } else {
        transition_ratio_log = acceptance(tuning) - acceptance(tuning)
      }
      posterior_ratio_log = posteriorNextState - posteriorCurrentState
      acceptance_probability = exp(posterior_ratio_log + transition_ratio_log)

      # decide on proposal:
      # a) go of probability of next state is higher
      # b) else go anyway probabalistically
      rndm = runif(1, 0, 1)
      if(!is.na(acceptance_probability) & (rndm < acceptance_probability)) {
        mu = muNext
        sigma = sigmaNext
        # just keep the new current posterior, as it will be needed in the next iteration
        # anyway and we do not want to calculate it anew
        posteriorCurrentState = posteriorNextState # changed
        
        if (i >= burnIn) {
          num_accept <- num_accept + 1 
        }
      }
      
      # after burn-in save sample in chain
      if (i >= burnIn) {
        out[c, i-burnIn, 1] = mu
        out[c, i-burnIn, 2] = sigma
      }
    }
  }
  
  # for future use, the thing we return is a coda mcmc.list
  mcmc_list <- vector("list", chains)
  for (c in 1:chains) {
    mcmc_list[[c]] <- mcmc(out[c,,])
  }
  return(mcmc.list(mcmc_list))
}



