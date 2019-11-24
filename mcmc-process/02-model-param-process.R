# For a given model, compute the median, mean, and maxLL values to put into a simple form


library(tidyverse)
# Files coded by model and treatment name

#models_name = "arrhenius|microbes|dead_soil"
#models_list="arrhenius-([[:upper:]]{3})|microbes-([[:upper:]]{3})|dead_soil-([[:upper:]]{3})"

models_name = "dead_soil"
models_list="dead_soil-([[:upper:]]{3})"

in_files <- list.files('mcmc-results',pattern=models_list,full.names = TRUE)




values <- list()
max_ll <- list()
mean_ll <- list()   # The mean of the likelihood

for (i in 1:length(in_files)) {
  load(in_files[i])

  curr_model <- str_extract(in_files[i],pattern=models_name)
  model <- str_extract(in_files[i],pattern=models_list)
  treatment <- str_extract(in_files[i],pattern='\\d')
  # Join accepted to values
  accepted <- estimate_results[[2]] %>%
    mutate(iteration=1:n()) %>%
    rename(accept=value)


  # Filter out on estimated parameters
  param_file_name <- paste0('param-defaults/',curr_model,'-param.csv')
  param_defaults <- read_csv(param_file_name) %>%
    mutate_if(is.integer,as.double) %>%
    select(name,changeable)



  # Join accepted to values, then filter on estimated parameters
  values[[i]] <- estimate_results[[1]] %>%
    inner_join(accepted,by="iteration") %>%
    filter(accept) %>%
    select(-accept) %>%
    inner_join(param_defaults,by="name") %>%
    mutate(model=model, treatment = treatment)


    max_ll_index <- estimate_results[[4]]

    max_ll[[i]] <- estimate_results[[1]] %>%
      inner_join(accepted,by="iteration") %>%
      filter(iteration == max_ll_index) %>%
      mutate(model=model, treatment = treatment, type="max_ll") %>%
      select(name,value,model,treatment,type)



}


model_median <- values %>% bind_rows() %>%
  group_by(model,treatment,name) %>%
  summarize(value=median(value)) %>%
  mutate(type="median") %>%
  select(name,value,model,treatment,type)

model_mean <- values %>% bind_rows() %>%
  group_by(model,treatment,name) %>%
  summarize(value=mean(value)) %>%
  mutate(type="mean") %>%
  select(name,value,model,treatment,type)

model_max_ll <- max_ll %>% bind_rows()

model_param_results <- bind_rows(model_median,model_max_ll,model_mean) %>%
  separate(model,c("model","site"),sep="-")  # Separate out the model and site columns

save(model_param_results,file='mcmc-results/model-parameter-results.Rda')

