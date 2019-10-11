# Now to run this we can just invoke a given map - yay!
mcmc_mode <- mcmc_defaults
treatment_start <- 1


models=c("arrhenius","microbes","dead_soil")
#models=c("arrhenius","microbes")
#models=c("microbes")
for (i in 1:length(models)) {
  mcmc_treatment(models[i],mcmc_mode,treatment_start)
  print(paste0("Ending the model runs for: ", models[i]))
}
