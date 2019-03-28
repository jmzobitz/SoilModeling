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

rapid_data <- read_csv(inputFluxesFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a format
  rename(site=SITE) %>%
  group_by(site,PLOTID) %>%  # Group by the plot and the day
  summarize(tot_obs = n(),
            Tsoil_C=mean(Tsoil_C,na.rm=TRUE),
            rSoil=mean(EFFLUX,na.rm=TRUE)* (1e-6 / 1) * (12 / 1 ) * (86400),
            rSoilErr = stderr(EFFLUX)* (1e-6 / 1) * (12 / 1 ) * (86400),  ### Change efflux from umol C02 m-2 s-2 to gC m-2 day-1
            beetles=sum(Beetle_CODE)>0, # Determine if we have beetles and fire by summing up and seeing if we get a numeric value.
            fire = sum(Fire_CODE)>0) %>%
  filter(!is.na(PLOTID)) %>% ungroup  # Remove any NA on the plots


use_data(rapid_data,overwrite = TRUE)

# Now analyze the microbe data
inputMicrobeFileLocation <- 'data-raw/HighPark_BGCpools_MASTER_r.csv'

microbe_data <- read_csv(inputMicrobeFileLocation) %>%
  mutate(Date = ymd(Date)) %>% # Convert date to a formate
  mutate(microbeC = Biomass_C_final*100*1e-6,  # Convert the soil Carbon and soil microbes from ug C to g soil
         soilC = DOC_Final*100e-6) %>% # Assume we have 100 g of soil in the measurement. 1 ug = 1e-6 g (in the notes High park)
  rename(site=SITE,soilWater = SoilMoisture_pcent) %>%
  select(Date,site,PLOTID,microbeC,soilC,soilWater) %>%
  group_by(site,PLOTID) %>%
  summarize(microbeC = mean(microbeC),soilC = mean(soilC),soilWater = mean(soilWater)) %>% ungroup()

use_data(microbe_data,overwrite = TRUE)


# Pull in the data that we need to do this problem, using only the microbe an soil data
flux_data <- rapid_data %>% inner_join(select(microbe_data,site,PLOTID,soilC,microbeC,soilWater),by=c("PLOTID","site"))


# Now compute the Q10 value at each site, using the regressions we found from enzyme-analysis.R
regressions <- Q10_temperature %>% filter(site %in% unique(flux_data$site)) %>% split(.$site)
temperature <- flux_data %>% select(site,PLOTID,Tsoil_C) %>% split(.$site)
Q10Effect <- map2(regressions,temperature,
                  ~(data.frame(PLOTID=.y$PLOTID,tempEffect=(.x$intercept+.x$slope*.y$Tsoil_C)^(.y$Tsoil_C/10))
                  )
)  %>%
  bind_rows(.id="site")


# Add the temperature effect columns to the data
flux_data <- cbind(flux_data,select(Q10Effect,tempEffect))


# Add in a column to flux_data for the different cases of beetle and fire data, which we call treatments
treatment_key <- expand(flux_data, nesting(beetles, fire)) %>%
  mutate(treatment = 1:n())

# Join the treatment data to the flux data
flux_data %>% inner_join(treatment_key,by=c("beetles","fire")) -> flux_data


use_data(flux_data,overwrite = TRUE)


# Save the cases as well
use_data(treatment_key,overwrite = TRUE)


