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

# rapid_data <- read_csv(inputFluxesFileLocation) %>%
#   mutate(Date = ymd(Date)) %>% # Convert date to a format
#   rename(site=SITE) %>%
#   group_by(site,PLOTID,Date) %>%  # Group by the plot and the day
#   summarize(tot_obs = n(),
#             Tsoil_C=mean(Tsoil_C,na.rm=TRUE),
#             rSoil=mean(EFFLUX,na.rm=TRUE)* (1e-6 / 1) * (12 / 1 ) * (86400),
#             rSoilErr = stderr(EFFLUX)* (1e-6 / 1) * (12 / 1 ) * (86400),  ### Change efflux from umol C02 m-2 s-2 to gC m-2 day-1
#             beetles=sum(Beetle_CODE)>0, # Determine if we have beetles and fire by summing up and seeing if we get a numeric value.
#             fire = sum(Fire_CODE)>0) %>%
#   filter(!is.na(PLOTID)) %>% ungroup  # Remove any NA on the plots


rapid_data <- read_csv(inputFluxesFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a format
  rename(site=SITE) %>%
#  group_by(site,PLOTID,Date) %>%  # Group by the plot and the day
  mutate(rSoil=EFFLUX* (1e-6 / 1) * (12 / 1 ) * (86400),
            rSoilErr = 0.3* (1e-6 / 1) * (12 / 1 ) * (86400),  ### Change efflux from umol C02 m-2 s-2 to gC m-2 day-1   # Set to be a
            beetles=(Beetle_CODE)>0, # Determine if we have beetles and fire by summing up and seeing if we get a numeric value.
            fire = (Fire_CODE)>0) %>%
  filter(!is.na(PLOTID)) %>%
  select(site,PLOTID,Tsoil_C,rSoil,rSoilErr,beetles,fire) %>% ungroup  # Remove any NA on the plots



use_data(rapid_data,overwrite = TRUE)
