### Author: JMZ
### Modified: 3/17/19
### Purpose: read in and process the raw data files - for use by the package owners

### Note: for file paths this assumes the working directory is the head package directory (soil modeling)

library(tidyverse)
library(devtools)
library(lubridate)

# Define function for standard error
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

inputFluxesFileLocation = 'data-raw/RAPID2013_MasterFluxes.csv'

flux_data <- read_csv(inputFluxesFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a formate
  group_by(PLOTID,Date) %>%  # Group by the plot and the day
  summarize(tot_obs = n(),
            Tsoil_C=mean(Tsoil_C,na.rm=TRUE),
            rSoil=mean(EFFLUX,na.rm=TRUE)* (1e-6 / 1) * (12 / 1 ) * (86400),
            rSoilErr = stderr(EFFLUX)* (1e-6 / 1) * (12 / 1 ) * (86400),  ### Change efflux from umol C02 m-2 s-2 to gC m-2 day-1
            beetles=sum(Beetle_CODE)>0, # Determine if we have beetles and fire by summing up and seeing if we get a numeric value.
            fire = sum(Fire_CODE)>0)


use_data(flux_data,overwrite = TRUE)

# Now analyze the microbe data
inputMicrobeFileLocation <- 'data-raw/HighPark_BGCpools_MASTER_r.csv'

microbe_data <- read_csv(inputMicrobeFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a formate
  mutate(microbeC = Biomass_C_final*100*1e-6,  # Convert the soil Carbon and soil microbes from ug C to g soil
         soilC = DOC_Final*100e-6) %>% # Assume we have 100 g of soil in the measurement. 1 ug = 1e-6 g (in the notes High park)
  rename(site=SITE,soilWater = SoilMoisture_pcent) %>%
  select(Date,site,PLOTID,microbeC,soilC,soilWater)

use_data(microbe_data,overwrite = TRUE)





