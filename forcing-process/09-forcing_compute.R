library(SoilModeling)
library(tidyverse)

# We will do the soil model - pull up the parameter results (median, max LL)
# We don't have the microbes or quality models set yet ....

# load up estimated parameters:
load('mcmc-results/model-parameter-results.Rda')

site_name = 'UMC'

# Function that computes the ratio in girdled to  at a given site, across the treatments, models, and parameter estimation type

# Helper function that evaluates across each model for a given site
flux_site_compute <- function(site_name) {

  # Filter out the param values by site, then distinguish across different levels
  filtered_params <- model_param_results %>% filter(site==site_name) %>%
    expand(nesting(type,model,treatment)) %>%
    mutate(vals = 1:n())

  joined_values <- model_param_results %>% filter(site==site_name) %>%
    left_join(filtered_params,by=c("treatment","model","type")) %>%
    split(.$vals)

  # Now map across the different model treatments

  model_out <- joined_values %>% map(.f=~model_evaluate(.x)) %>%
    bind_rows(.id="vals")  # %>%
  #  mutate(vals=as.integer(vals)) %>%
  #  rename(model=model_id) %>%
  #  left_join(filtered_params,by=c("vals","treatment","model"))

  return(model_out)
}











# Helper function that evaluates the model for a given parameter set
model_evaluate <- function(param_in) {

  model_id_char <- param_in$model[1]

  # Define the function we will use in the process
  eval(parse(text=(paste0("model_fn<-",model_id_char))))

  # Now we need to loop through each of the values.

  # Take the starting soil value as the mean across all sites
  curr_soilC_normal <- mean(flux_data$soilC)
  curr_soilC_girdle <- curr_soilC_normal



  # Define the fluxes
  r_out_girdle <- array(0,dim=dim(niwot_forcing_girdle)[1])
  r_out_normal <- array(0,dim=dim(niwot_forcing_girdle)[1])
  f_litter <- 146/365  # The daily input of litter (146 gC / year)

  for(i in 1:dim(niwot_forcing_girdle)[1]) {

    # Compute Girdled fluxes
    data_in <- niwot_forcing_girdle[i,]
    data_in$soilC=curr_soilC_girdle
    gpp_in <- data_in$GPP

    r_out_girdle[i] <- model_fn(params_in,data_in)$value[1]
    curr_soilC_girdle <- max(curr_soilC_girdle + 0.1*gpp_in + f_litter - r_out_girdle[i],0)

    # Compute normal fluxes
    data_in <- niwot_forcing_normal[i,]
    data_in$soilC=curr_soilC_normal
    gpp_in <- data_in$GPP

    r_out_normal[i] <- model_fn(params_in,data_in)$value[1]
    curr_soilC_normal <- max(curr_soilC_normal + 0.1*gpp_in + f_litter - r_out_normal[i],0)

  }

  flux_out <- data.frame(soil_ratio = 1-r_out_girdle/r_out_normal )
  # Then return the percent change:
  return(flux_out)

}

