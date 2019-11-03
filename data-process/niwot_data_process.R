### Get out an annual average of fluxnet data from NIWOT
library(devtools)
library(tidyverse)
library(lubridate)

# The directory that contains all the Niwot data
fileDir <- 'data-raw/ameriflux_data_ver.2018.11.28'  # This needs to be better specified - or wherever we are saving the data


# List files that we are working with (flags, and climate data)
fileList_flags <- list.files(fileDir,pattern = 'climate_flags',recursive = TRUE,include.dirs = TRUE,full.names = TRUE)


fileList <- list.files(fileDir,pattern = 'climate_([[:digit:]])',recursive = TRUE,include.dirs = TRUE,full.names = TRUE)

niwot_data <- list()

# Loop through all the years and then select out the soil water and temperature columns
for(i in 1:length(fileList)) {
  fluxData <- read_delim(fileList[i],comment="%",delim=" ",col_names = FALSE,trim_ws = TRUE) %>% mutate(X2 = as.numeric(X2),X3 = as.numeric(X3), X4 = as.numeric(X4))   %>%
    select(c(1:3,7,19,34)) %>%
    rename(year=1,month=2,day=3,decimal_day=4,Tsoil_C=5,soilWater=6)

  qcData <- read_delim(fileList_flags[i],comment="%",delim=" ",col_names = FALSE,trim_ws = TRUE) %>% mutate(X3 = as.numeric(X3),X4 = as.numeric(X4)) %>%
    select(c(1:3,7,18,30)) %>%
    rename(year=1,month=2,day=3,decimal_day=4,Tsoil_C_qc=5,soilWater_qc=6)


  for(j in 5:6) {
    qc_current <- qcData %>%
      select(j) %>%
      pull() # remove QC

    qc_current_test <- qc_current ==1
    fluxData <- fluxData %>%
      mutate_at((j),~ifelse(qc_current_test,.,NA))
  }


  niwot_data[[i]] <- fluxData

}


avg_soil <- bind_rows(niwot_data) %>% group_by(floor(decimal_day)) %>%
  summarize_at(c("Tsoil_C","soilWater"),mean,na.rm=TRUE) %>%
  rename(day=1) %>%
  filter(day!=366)

# Now compute the temperature effect by doing the weighted Q10 value and the median across all
nwt_q10 <- weighted_Q10 %>% filter(str_detect(PLOTID,"NWT")) %>%
  summarize(median(Q10)) %>% pull()

# Link this to the la_thuille data set (which calculated GPP) from the GiniModeling package
niwot_average <- GiniCoeff::la_thuille %>%
  filter(site == "US-NR1") %>%
  mutate(day = yday(time)) %>%
  group_by(day) %>%
  summarise(GPP = mean(GPP_NT_VUT_REF)) %>%
  filter(day!=366) %>%
  left_join(avg_soil,by="day") %>%
  mutate(tempEffect = nwt_q10^(Tsoil_C/10))






# Save the cases as well
use_data(niwot_average,overwrite = TRUE)
