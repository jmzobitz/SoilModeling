# What we want to do is to load up the models and then for each of the summary statistics compute the modeled values of rSoil based on the model.  We use these for a Taylor diagrams

### We build this on three levels of the map function:
# 1) By parameter estimate (maxLL, median values) --> Largest level
# 2) By model structure (dead_soil, microbes, arrhenius)
# 3) By treatment  (beetles / fire) --> smallest level
# 4) By site

library(SoilModeling)
library(tidyverse)

# load up estimated parameters:
load('mcmc-results/model-parameter-results.Rda')


# Create a tag for each of the measurements
in_flux_data <- flux_data %>% mutate(measurement = 1:n())

# Function that computes the Rsoil at a given site, across the treatments, models, and parameter estimation type
rSoil_site_compute <- function(site_name) {

  # Filter out the param values by site, then distinguish across different levels
  filtered_params <- model_param_results %>% filter(site==site_name) %>%
    expand(nesting(type,model,treatment)) %>%
    mutate(vals = 1:n())

  joined_values <- model_param_results %>% filter(site==site_name) %>%
    left_join(filtered_params,by=c("treatment","model","type")) %>%
    split(.$vals)

  # Now map across the different model treatments

  model_out <- joined_values %>% map(.f=~soil_evaluate(.x)) %>%
    bind_rows(.id="vals") %>%
    mutate(vals=as.integer(vals)) %>%
    rename(model=model_id) %>%
    left_join(filtered_params,by=c("vals","treatment","model"))

  return(model_out)
}




### DEFINE SOME HELPER FUNCTIONS:
# LEVEL Define a function that just allows us to evaluate a given model and structure
soil_evaluate <- function(param_in) {
  # split this by treatment


  # These should be the same for each one that we do
  model_id_char <- param_in$model[1]
  site_name <- param_in$site[1]


  data_list <- in_flux_data %>%
    filter(site==site_name) %>%
    split(.$treatment)

  param_list <- param_in %>%
    split(.$treatment)

  eval(parse(text=(paste0("model_fn<-",model_id_char))))

  out_val <- map2(.x=param_list,.y=data_list,~{
    out_flux <- model_fn(.x,.y) %>%
      mutate(measurement=.y$measurement)
    return(out_flux)
    }) %>%
    bind_rows(.id="treatment") %>%
    mutate(model_id=model_id_char) %>%
    rename(modeled = value)

  return(out_val)
}

### Now let's start to bring this all together

site_names <- unique(model_param_results$site)

modeled_rSoil <- model_param_results %>% split(.$site) %>%
  map(.f=~rSoil_site_compute(.x$site[1])) %>%
  bind_rows() %>%
  left_join(select(in_flux_data,rSoil,measurement),by=c("measurement")) %>%
  rename(measured=rSoil)

save(modeled_rSoil,file='mcmc-results/modeled-rSoil-results.Rda')

