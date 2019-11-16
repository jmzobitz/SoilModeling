library(SoilModeling)
library(tidyverse)

# We will do the soil model



model_id <- 'dead_soil'
### Parameters need to be input


param_file_name <- paste0('param-defaults/',model_id,'-param.csv')
params <- read_csv(param_file_name)

# Specify the model_function
eval(parse(text=(paste0("model_fn<-",model_id))))


# Now we need to loop through each of the values.


curr_soilC_normal <- .027
curr_soilC_girdle <- curr_soilC_normal

r_out_girdle <- array(0,dim=dim(niwot_forcing_girdle)[1])
r_out_normal <- array(0,dim=dim(niwot_forcing_girdle)[1])
f_litter <- 146/365  # The daily input of litter (146 gC / year)

params_in <- params_new
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

# Then return the percent change:
return(1-r_out_girdle/r_out_normal)
