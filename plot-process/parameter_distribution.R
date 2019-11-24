# Collect and make a boxplot distribution of accepted parameters, reports back the median and summary
# statistic in a file

library(tidyverse)
# Files coded by model and treatment name


models_name = "arrhenius|microbes|dead_soil"
models_list="arrhenius-([[:upper:]]{3})|microbes-([[:upper:]]{3})|dead_soil-([[:upper:]]{3})"

in_files <- list.files('mcmc-results',pattern=models_list,full.names = TRUE)



values <- list()

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
  filter(changeable) %>%
  select(-changeable) %>%
  mutate(model=model, treatment = treatment) %>%
  separate(model,c("model","site"),sep="-")   # Set out the site and model name






}

# Make a boxplot of estimated parameters
plot_data <- values %>% bind_rows() %>% split(.$model)

## We need to report the quantile for each parameter




for (i in 1:length(plot_data)) {

  modelName <- head(plot_data[[i]]$model,1)
  fileName <- paste0('manuscript-figures/', modelName, '-histograms.png')


  # Make a boxplot of estimated parameters
 # curr_plot <-  yee[[i]] %>%
#    ggplot() +
#    geom_freqpoly(aes(x=value,stat(density),color=treatment),bins=100) +facet_grid(.~name,scales = "free")

 curr_plot <- plot_data[[i]] %>%
    ggplot(aes(x=treatment,y=value,color=site)) +
    geom_boxplot() +
  #  geom_jitter(size=1,aes(color=treatment)) +
    facet_grid(.~name,scales = "free") + coord_flip()

    nParams = length(unique(plot_data[[i]]$name))
  ggsave(fileName,plot=curr_plot,width=nParams*3,height=3)




}



### Next report out the values in a summary table for ease of use:

# Helper function to compute the statistics we need
my_stats <- function(in_data) {

  q_vals <- quantile(in_data$value,probs=c(0.025,0.5,0.975))

  out_df <- data.frame(mean=mean(in_data$value),
                       q025=q_vals[1],
                       q50=q_vals[2],
                       q975=q_vals[3])

  return(out_df)
}


# Organize everything my the parameter name using nest
by_param_name <-  values %>% bind_rows() %>%
  group_by(site,treatment,model,name) %>%
  nest()

# Then we will comput the statistics
out_table <- by_param_name %>%
  mutate(fav_stats = map(data, my_stats)) %>%
  unnest(fav_stats) %>%
  select(-data)

# Save these to a file
write_csv(out_table,path='manuscript-figures/param-results.csv',col_names = TRUE)

#

