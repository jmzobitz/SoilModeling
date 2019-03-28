# Test run on linear data

x = seq(0,20,length.out=300)
y = 3*x + rnorm(length(x),sd=2)

# read in parameters

library(tidyverse)

param_defaults <- read_csv('param-defaults/linear-param.csv')
# Identify our model
model <- linear_test
likelihood <- likelihood_linear
data_in <- y



join_key <- "PLOTID"  # What we will do to join the data with the modeled values
iterations <- 100 #mcmc_defaults$NUM_AT_ONCE
tuning <- TRUE
burn_chain <- FALSE
max_iterations <- 500 # mcmc_defaults$MAX_ITER


param_defaults

param_in <- initialize(param_defaults)
mcmc_chain<-function(param_defaults) {
  iter_curr <- 0  # variable keeping track how many iterations we've done.
  accept_val <- 0


  param_in <- initialize(param_in)

  while ( abs(accept_val-mcmc_defaults$A_STAR)>mcmc_defaults$THRESH & iter_curr < max_iterations ) {
    curr_results <- metropolis_chain(param_in,data_in,model,join_key,iterations,tuning,burn_chain)
    iter_curr <- iter_curr+iterations  # Update number of iterations
    accept_val <- curr_results[[2]] # Update to last parameters
    param_in <- curr_results[[1]]

  }

  return(curr_results)
}
