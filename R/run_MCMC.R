#' Do a metropolis chain
#'
#' \code{run_MCMC} Does a complete MCMC parameter estimation
#'
#' @param param_file_name file name (as a string) of where the default parameters are located
#' @param model_in the name of the model we are using
#' @param data_in input data we use to compare in the likelihood function.
#' @param join_key The name of the variable we will join to data_in when doing likelihood
#' @param results_file_name The file name (as a string) where the final MCMC results will be stored.
#' @param out_file_name The file name (as a string) where we log various info as the mcmc proceeds
#' @param mcmc_parameters a data frame of how we want to estimate (can be mcmc_superfast or mcmc_defaults)
#'
#'
#' @import dplyr
#' @export


run_MCMC <- function(param_file_name,model_in,data_in,join_key,results_file_name,out_file_name,mcmc_parameters) {

  # A function that runs a chain MCMC chain for tuning
  mcmc_chain_tuning<-function(param_defaults) {
    iter_curr <- 0  # variable keeping track how many iterations we've done.
    accept_val <- 0

    param_in <- initialize(param_defaults)


    # Tune a chain until it gets converged
    while ( abs(accept_val-mcmc_parameters$A_STAR)>mcmc_parameters$THRESH & iter_curr < mcmc_parameters$MAX_ITER) {
      curr_results <- metropolis_chain(param_in,data_in,model,join_key,iterations,tuning,FALSE,mcmc_parameters)
      iter_curr <- iter_curr+iterations  # Update number of iterations
      accept_val <- curr_results[[2]] # Update to last parameters
      param_in <- curr_results[[1]]
      ll_val <- curr_results[[3]]
      # Print results to the output files
      write(c(paste("Iteration: ", iter_curr, round(100*accept_val,digits=3), "% accepted at ", date())), out_file_name,append=TRUE)


    }

    # Print parameter values to the output files
    out_data_frame <- curr_results[[1]] %>%
      select(name,changeable,value,knob) %>%
      mutate(value=round(value,digits=3),knob=round(knob,digits=3))

    write(c(""), out_file_name,append=TRUE)
    write(c(paste("Max LL of chain: ", round(ll_val,digits=3))), out_file_name,append=TRUE)
    write(c(""), out_file_name,append=TRUE)
    write(c("Parameter values:"), out_file_name,append=TRUE)

    write(c(paste("Parameter", "Estimated","CurrentValue","Knob")), out_file_name,append=TRUE)
    write.table(out_data_frame, file=out_file_name,sep="\t",append=TRUE,row.names=FALSE,quote=FALSE,col.names=FALSE)
    write(c(""), out_file_name,append=TRUE)

    return(curr_results)
  }


  # Identify our parameters and our model, convert to make sure we don't have type problems
  param_defaults <- read_csv(param_file_name) %>% mutate_if(is.integer,as.double)
  model <- model_in

  # Set the tuning and the estimate iterations
  iterations <- mcmc_parameters$NUM_AT_ONCE
  tuning <- mcmc_parameters$CHAIN_TUNING
  max_iterations <- mcmc_parameters$MAX_ITER
  burn_iterations <- mcmc_parameters$BURN_ITER
  estimate_iterations <- mcmc_parameters$ESTIMATE_ITER
  n_chains <- mcmc_parameters$NCHAINS


  ### Header info on out files:
  # Write the initial values
  write(c("Starting Parameter Estimation"), out_file_name)
  write(c(paste("Input parameter file: ",param_file_name)), out_file_name,append=TRUE)
  #write(c(paste("Input data file: ",dataFileName)), outFileName,append=TRUE)
  write(c(paste("Output parameter estimate file: ",results_file_name)), out_file_name,append=TRUE)
  write(c(""), out_file_name,append=TRUE)
  write(c(paste("Number of Chains: ",n_chains)), out_file_name,append=TRUE)
  write(c(paste("Max Iterations per Chain: ",mcmc_parameters$MAX_ITER)), out_file_name,append=TRUE)
  write(c(paste("Burn Iterations / Estimate Iterations: ",burn_iterations, " / ", estimate_iterations)), out_file_name,append=TRUE)
  #write(c(paste("MCMC Random Number Generator Seed value: ",seedValue)), outFileName,append=TRUE)
  write(c(""), out_file_name,append=TRUE)

  # Start tuning up the chains:
  # Make a list to store the chain results
  chain_results <- list()

  for (i in 1:n_chains) {

    write(c(paste("Chain", i, "of ", n_chains)), out_file_name,append=TRUE)
    write(paste("Start Chain Time:", date()),out_file_name,append=TRUE)
    chain_results[[i]] <- mcmc_chain_tuning(param_defaults)

    write(paste("End Chain Time:", date()),out_file_name,append=TRUE)
    write(c("***********************"), out_file_name,append=TRUE)
  }

  # Now pick the max likelihood value across our chains:
  max_ll_index <- lapply(chain_results, `[`, 3) %>%
    unlist %>%
    which.max

  # Set the parameter values to the maxLL chain:
  estimating_param <- chain_results[[max_ll_index]][1] %>% data.frame()

  # Starting the estimate chain - first we do a burn-in
  write(c(paste("Starting parameter estimation from maxLL parameters from chain: ", max_ll_index),""), out_file_name,append=TRUE)


  write(paste("Start Chain Time:", date()),out_file_name,append=TRUE)


  # Do a burn in chain and write out the values
  burn_results <- metropolis_chain(estimating_param,data_in,model,join_key,burn_iterations,FALSE,FALSE,mcmc_parameters)

  write(paste("End Chain Time:", date()),out_file_name,append=TRUE)
  write(c(paste("Burn chain acceptance: ", round(100*burn_results[[2]],digits=3), "% accepted ")), out_file_name,append=TRUE)
  write(c(""), out_file_name,append=TRUE)

  # Do an estimate chain
  write(c(paste("Starting estimate chain.")), out_file_name,append=TRUE)
  write(paste("Start Chain Time:", date()),out_file_name,append=TRUE)
  estimate_results <- metropolis_chain(burn_results[[1]],data_in,model,join_key,estimate_iterations,FALSE,TRUE,mcmc_parameters)
  estimate_accept <- estimate_results[[2]] %>% summarize(sum(value)/n()) %>% pull
  write(c(paste("Estimate chain acceptance: ", round(100*estimate_accept,digits=3), "% accepted ")), out_file_name,append=TRUE)
  write(paste("End Chain Time:", date()),out_file_name,append=TRUE)

  # Save the parameter values
  save(estimate_results,file=results_file_name)


}

















