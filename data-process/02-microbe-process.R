### Author: JMZ
### Modified: 3/17/19
### Purpose: read in and process the raw data files - for use by the package owners

### Note: for file paths this assumes the working directory is the head package directory (soil modeling)

library(tidyverse)
library(devtools)
library(lubridate)



# Now analyze the microbe data
inputMicrobeFileLocation <- 'data-raw/HighPark_BGCpools_MASTER_r.csv'

microbe_data <- read_csv(inputMicrobeFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a formate
  mutate(microbeC = Biomass_C_final*100*1e-6,  # Convert the soil Carbon and soil microbes from ug C to g soil
         soilC = DOC_Final*100e-6) %>% # Assume we have 100 g of soil in the measurement. 1 ug = 1e-6 g (in the notes High park)
  rename(site=SITE,soilWater = SoilMoisture_pcent) %>%
  select(Date,site,PLOTID,microbeC,soilC,soilWater) # %>%
 # group_by(site,PLOTID) %>%
#  summarize(microbeC = mean(microbeC),soilC = mean(soilC),soilWater = mean(soilWater)) %>% ungroup()

use_data(microbe_data,overwrite = TRUE)
