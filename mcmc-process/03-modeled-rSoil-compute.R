# What we want to do is to load up the models and then for each of the summary statistics compute the modeled values of rSoil based on the model.  We use these for a Taylor diagrams

### We build this on three levels of the map function:
# 1) By parameter estimate (maxLL, median values) --> Largest level
# 2) By model structure (dead_soil, microbes, arrhenius)
# 3) By treatment  (beetles / fire) --> smallest level

library(SoilModeling)
library(tidyverse)

# load up estimated parameters:
load('mcmc-results/model-parameter-results.Rda')

# Define the models we are working with.
model_names=c("arrhenius","microbes","dead_soil")

# LEVEL 3: Define a function that just allows us to evaluate a given model and structure
soil_evaluate <- function(param_in,model_id) {
  # split this by treatment
  model_id_char <- model_id %>% pull(names)
  param_list <- param_in %>%
    filter(model==model_id) %>%
    split(.$treatment)

  data_list <- flux_data %>% split(.$treatment)

  eval(parse(text=(paste0("model_fn<-",model_id_char))))

  out_val <- map2(.x=param_list,.y=data_list,~model_fn(.x,.y)) %>%
    bind_rows(.id="treatment") %>%
    mutate(model_id=model_id_char) %>%
    rename(modeled = value)

  return(out_val)
}

# LEVEL 2: Now define an mid-level function that computes results for the model names
model_evaluate <- function(param_list,model_values) {
  model_out <- map(model_values,~soil_evaluate(param_list,.x)) %>%
    bind_rows(.id="names")
  return(model_out)
}

# LEVEL 1: Define a function that computes this for the parameter list
type_evaluate <- function(param,model_list) {
  type_out <- map(param,~model_evaluate(.x,model_list))
  return(type_out)
}

###  Now loop on the parameter structure
type_list <- model_param_results %>% split(.$type)
models <- data.frame(names = model_names) %>% split(.$names)

# Compute the output
modeled_rSoil <- type_evaluate(type_list,models) %>%
  bind_rows(.id="type")  %>%
  left_join(select(flux_data,PLOTID,rSoil),by="PLOTID") %>%
  rename(measured=rSoil)


save(modeled_rSoil,file='mcmc-results/modeled-rSoil-results.Rda')

