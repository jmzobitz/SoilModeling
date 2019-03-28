# read in parameters

library(tidyverse)

param_file_name <- 'param-defaults/dead_soil-param.csv'


# Spilt off the flux data by treatment
flux_data_treatment <- flux_data %>% split(.$treatment)  # We've split out the data by treatment

# Generate a list of log files
log_files <-  list(
  file_loc = "mcmc-log-files/",
  id = "dead_soil-",
  treatment = unique(flux_data$treatment),
  ending=".out"
) %>%
  cross_df() %>%
  mutate(file_name = paste0(file_loc,id,treatment,ending)) %>%
  select(file_name,treatment) %>%
  split(.$treatment)

# Generate a list of result files
results_files <-  list(
  file_loc = "mcmc-results/",
  id = "dead_soil-",
  treatment = unique(flux_data$treatment),
  ending=".Rda"
) %>%
  cross_df() %>%
  mutate(file_name = paste0(file_loc,id,treatment,ending)) %>%
  select(file_name,treatment) %>%
  split(.$treatment)


# Now let's map these out - fingers crossed!
pmap(list(x=log_files,y=results_files,z=flux_data_treatment),
     .f=function(x,y,z){ run_MCMC(param_file_name,dead_soil,z,"PLOTID",y$file_name,x$file_name,mcmc_superfast) })




