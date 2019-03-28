# Collect and make a boxplot distribution of accepted parameters
library(tidyverse)
# Files coded by model and treatment name
in_files <- list.files('mcmc-results',full.names = TRUE)
models_list="arrhenius|microbes|dead_soil"


values <- list()

for (i in 1:length(in_files)) {
load(in_files[i])

model <- str_extract(in_files[i],pattern=models_list)
treatment <- str_extract(in_files[i],pattern='\\d')
# Join accepted to values
accepted <- estimate_results[[2]] %>%
  mutate(iteration=1:n()) %>%
  rename(accept=value)


# Filter out on estimated parameters
param_file_name <- paste0('param-defaults/',model,'-param.csv')
param_defaults <- read_csv(param_file_name) %>%
  mutate_if(is.integer,as.double) %>%
  select(name,changeable)



# Join accepted to values, then filter on estimated parameters
values[[i]] <- estimate_results[[1]] %>%
  inner_join(accepted,by="iteration") %>%
  filter(accept) %>%
  select(-accept) %>%
  inner_join(param_defaults,by="name") %>%
  filter(changeable) %>%
  select(-changeable) %>%
  mutate(model=model, treatment = treatment)






}

# Make a boxplot of estimated parameters
yee <- values %>% bind_rows() %>% split(.$model)

for (i in 1:length(yee)) {

  modelName <- head(yee[[i]]$model,1)
  fileName <- paste0('manuscript-figures/', modelName, '-histograms.png')


  # Make a boxplot of estimated parameters
 # curr_plot <-  yee[[i]] %>%
#    ggplot() +
#    geom_freqpoly(aes(x=value,stat(density),color=treatment),bins=100) +facet_grid(.~name,scales = "free")

 curr_plot <- yee[[i]] %>%
    ggplot(aes(x=treatment,y=value)) +
    geom_boxplot() +
  #  geom_jitter(size=1,aes(color=treatment)) +
    facet_grid(.~name,scales = "free") + coord_flip()

    nParams = length(unique(yee[[i]]$name))
  ggsave(fileName,plot=curr_plot,width=nParams*3,height=3)




}





