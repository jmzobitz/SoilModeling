#' Do a metropolis chain
#'
#' \code{metropolis_chain} Runs a MCMC metropolis chain
#'
#' @param param_in input parameter vector - is a data frame with the following columns:
#' 1. name
#' 2. changeable
#' 3. value
#' 4. minVal
#' 5. knob
#' 6. sampled
#' @param data_in input data we use to compare in the likelihood function.
#' @param model the particular model we are using in the mcmc
#' @param join_key The name of the variable we will join to data_in when doing likelihood
#' @param iterations The number of times we want to iterate through this chain
#' @param tuning a T/F value depending on if we want to limit the searchable range of parameters
#' @param estimate_chain a T/F value depending on what we want to return.
#' @param mcmc_parameters a data frame of how we want to estimate (can be mcmc_superfast or mcmc_defaults)
#'
#' @return a list, depending on if we want to track parameter values and if they were accepted, or just the final value for updating.
#'
#' @import dplyr
#' @export

### Main MCMC routine.
metropolis_chain <- function(param_in,data_in,model,join_key,iterations,tuning,estimate_chain,mcmc_parameters){
  # tuning = TRUE ==> simulated annealing
  # tuning = FALSE ==> keep the knob as it is


  if (tuning == FALSE) {
    incVal = 1
    decVal = 1
  } else {
    incVal = mcmc_parameters$INC
    decVal = mcmc_parameters$DEC
  }

  maxLL = -99999;	# set this to a ridiculusly low number so we will accept the first one

  param_curr <- param_in
  param_curr$sampled = FALSE  # Add in this column for later
  seedSave = .Random.seed;   # Save the current state of the RNG seed, if it gets reset it in the model

  # Declare a list where we will store parameter values and if we accepted or not, depending on if we burn things or not



  if(estimate_chain) {
    param_iterations <- list()
    accepted <- list()
    ll_mean <- 0  # Running total for the mean likelihood

  } else {
    #param_iterations <- param_curr %>% select(name,value)
    accepted <- 0
  }

  prior_likelihood <- prior(param_curr)   # Since we assume it is uniform, we take out?

  likelihood_curr <- likelihood(model(param_curr,data_in),data_in)+prior_likelihood;



  maxLL <- max(maxLL,likelihood_curr)
  maxLL_iteration <- 0  # The place where the max likelihood is optimized


  ### Pre-allocate some vectors so we save time
  random_accept <- log(runif(iterations))
  param_sample_vector = sample(param_curr$name[param_curr$changeable],iterations,replace = TRUE)
  tune <- runif(iterations)-0.5;
  for (i in 1:iterations) {


    ###########
    acceptFlag <- FALSE
    # Propose a parameter
    #param_proposed <- sample_parameters(param_curr)
    ###
    param_proposed <- param_curr
    param_proposed$sampled = FALSE

    paramToChange = param_proposed %>% filter(name==param_sample_vector[i])
    paramToChange$sampled <- TRUE
    minVal <- paramToChange$minVal
    maxVal <- paramToChange$maxVal
    paramRange = maxVal - minVal
    value <- paramToChange$value
    knob <- paramToChange$knob
    paramToChange$value = (paramToChange$knob * paramRange * tune[i])+paramToChange$value
    #paramToChange$value=runif(1,min=max(minVal,value-0.5*knob*paramRange),max=min(maxVal,value+0.5*knob*paramRange))
    if(between(paramToChange$value,paramToChange$minVal,paramToChange$maxVal)) {

      # Compute the likelihood of this new proposed parameter
    #  likelihood_proposed <- likelihood(model(param_proposed,data_in),data_in)+prior(param_proposed)

    param_proposed[param_proposed$name==param_sample_vector[i],]=paramToChange

    likelihood_proposed <- likelihood(model(param_proposed,data_in),data_in)+prior_likelihood


      likelihood_diff <- likelihood_proposed - likelihood_curr
      if (random_accept[i] < likelihood_diff) { acceptFlag <- TRUE}
# # if pos, the current value is better.  If neg, we keep the proposed value.
#       # Do a test to see if
#       # print(likelihood_diff)
      #  if (likelihood_diff > 0) {
      #    acceptFlag = TRUE
      #  } else {
      #    acceptFlag = FALSE
      #  }  # Reject with random prop
      # if (!acceptFlag) {
      #   if( random_accept[i] < likelihood_diff) {acceptFlag=TRUE}
      # }

      # accept_prob <- min(0,likelihood_diff)
      # if (log(runif(1)) < accept_prob) {acceptFlag = TRUE}
      # else{acceptFlag = FALSE}
    # Evaluate if accept or reject

    }

    if (acceptFlag) {  # We accept the proposed value

      # Adjust the knob
      #knob_proposed <- max(param_proposed$knob*incVal,1e-8)
      #knob_proposed <- min(knob_proposed,1)
      #knob_proposed <- max(param_proposed$knob*incVal,1e-8)
      knob_proposed <- min(knob*incVal,1)
      param_curr <- param_proposed %>% # accept the proposed value
        mutate(knob = if_else(sampled,knob_proposed,knob), # Adjust the knob
               sampled = FALSE) # Reset if we sampled

      # Compute the new likelihood

      likelihood_curr <- likelihood_proposed

      # Update the max likelihood if we see it
      if( maxLL>likelihood_curr) {
        maxLL <- likelihood_curr
        maxLL_iteration <- i
      }


    } else {

      #knob_proposed <- max(param_curr$knob*decVal,1e-8)
      #knob_proposed <- min(knob_proposed,1)
      #knob_proposed <- max(param_curr$knob*decVal,1e-8)
      knob_proposed <- max(knob*decVal,1e-8)
      param_curr$sampled <- param_proposed$sampled  # Redefine sampled so we can adjust the know
      param_curr <- param_curr %>% # keep the accepted value
        mutate(knob = if_else(sampled,knob_proposed,knob), # Adjust the knob
               sampled = FALSE) # Reset if we sampled

    }

      if(estimate_chain) {
        # Write out the parameter values
        param_iterations[[i]] <- param_curr %>% select(name,value)
        # Determine if we have accepted or not
        accepted[[i]] <- data.frame(value=acceptFlag)
        ll_mean <- ll_mean+ likelihood_curr
      } else {
        accepted <- accepted + acceptFlag
      }

  }

  .Random.seed = t(seedSave);  # Load back up to seed the current state of RNG for MCMC

  if(estimate_chain) {   # If we are keeping the values them write them all out

    param_out <- param_iterations %>%
      bind_rows(.id="iteration") %>%
      mutate(iteration=as.numeric(iteration))

    accept <- accepted %>% bind_rows()
    ll_mean <- ll_mean / sum(accept$value)

    out_list <- list(param_out,accept,maxLL,maxLL_iteration,ll_mean)
  } else {  # We just want to keep the current values and the knob, reporting the acceptance

    param_out <- param_curr %>% select(-sampled)
    accept <- accepted /iterations
    out_list <- list(param_out,accept,maxLL)
  }

  return(out_list)
}



