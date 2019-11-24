library(tidyverse)
library(SoilModeling)

# Now to run this we can just invoke a given map - yay!
mcmc_mode <- mcmc_defaults
treatment_start <- 1


#models=c("dead_soil","arrhenius","microbes")
#models=c("arrhenius","microbes")
#models=c("microbes")
models=c("dead_soil")
for (i in 1:length(models)) {
  mcmc_treatment(models[i],mcmc_mode,treatment_start)
  print(paste0("Ending the model runs for: ", models[i]))
}
