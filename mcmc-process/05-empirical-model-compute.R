### Do a linear model between soil carbon and moisture for each treatment and site
### This just does an empirical model that we can use

## Very helpful: R4DS (Wickham) and the Broom package
library(tidyverse)
library(modelr)
library(broom)

# Define a linear function that will do the interactions on our data frame
linear_model  <- function(df) {
  lm(rSoil ~ Tsoil_C + soilWater + microbeC + soilC + soilC*microbeC + soilWater*Tsoil_C, data = df)
}

# Nest everything together by site and treatment
by_plot <- flux_data %>%
  group_by(site, treatment) %>%
  nest()



# Map across all the different models
by_plot <- by_plot %>%
  mutate(model=map(by_plot$data, linear_model))

modeled_values <- by_plot %>%
 # mutate(glance = map(model, broom::glance)) %>%  # This will do model fits
  mutate(augment = map(model,broom::augment)) %>%   # This will examine the predicted to measured
  unnest(augment)

# Compute the taylor values  (this is what we are using this for ... )
taylor_values_empirical <- modeled_values %>%
  group_by(site,treatment) %>%
  summarize(
    sd_meas = 1,
    sd_model = sd(.fitted) / sd(rSoil),
    r = cor(.fitted,rSoil),
    centered_rms = sd((rSoil-mean(rSoil))-((.fitted-mean(.fitted))))/sd(rSoil),
    x_coord = sd_model*r,
    y_coord = sd_model*sin(acos(r))
  ) %>%
  mutate(type='empirical',
         model='empirical',
         treatment = as.character(treatment))

save(taylor_values_empirical,file='mcmc-results/empirical-taylor-results.Rda')

