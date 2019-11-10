# Build the forcing dataset from niwot averages

library(SoilModeling)
library(tidyverse)
library(lubridate)


### Then we compute the soil at each timestep
# Build the forcing dataset from the niwot data
normal_years <- 5
girdle_years <- 4
base_exp <- 0.6
girdled_data <- map_dfr(seq_len(girdle_years), ~niwot_average,.id="year") %>%
  mutate(year=as.numeric(year)) %>%
  mutate(GPP =GPP*base_exp^(year-1+day/365),
         year = year+normal_years)

normal_data <- map_dfr(seq_len(normal_years), ~niwot_average,.id="year") %>%
  mutate(year=as.numeric(year))


# Then combine the dataset together

niwot_forcing_girdle <- rbind(normal_data,girdled_data) %>%
  mutate(site='simulate',
         PLOTID=5)

niwot_forcing_normal <- rbind(normal_data,normal_data) %>%
  mutate(site='simulate',
         PLOTID=5)

use_data(niwot_forcing_girdle,overwrite = TRUE)
use_data(niwot_forcing_normal,overwrite = TRUE)

#
