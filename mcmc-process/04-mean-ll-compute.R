# Loops across all the model results to compute the likelihood from the mean parameter values.  Useful in computing the DIC


library(SoilModeling)
library(tidyverse)

# load up estimated parameters:
load('mcmc-results/model-parameter-results.Rda')

###  Now loop on the treatment index for both the parameters and the data, using the mean parameters
mean_params <- model_param_results %>% filter(type=='mean') %>% split(.$treatment)
data_list <- flux_data %>% split(.$treatment)


# LEVEL 3: Define a function that computes the mean likelihood valuejust allows us to evaluate a given model and structure
likelihood_evaluate <- function(param_in,data_in) {
  model_names=c("arrhenius","microbes","dead_soil")
  models <- data.frame(names = model_names) %>% split(.$names)

 out_df <- map(.x=models,.f=~{ param_values <- param_in %>%
    filter(model == .x$names)
  eval(parse(text=(paste0("model_fn<-",.x$names))))
  h <- model_fn(param_values,data_in)
  z<- data.frame(mean_ll=likelihood(h,data_in))
  return(z)
 }) %>% bind_rows(.id="names")
  return(out_df)
}


### Now map across the different treatments:
mean_ll_values <- map2(.x=mean_params,.y=data_list,~likelihood_evaluate(.x,.y)) %>%
  bind_rows(.id="treatment")

save(mean_ll_values,file='mcmc-results/mean-ll-results.Rda')
