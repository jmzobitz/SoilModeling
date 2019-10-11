library(tidyverse)
library(devtools)
library(SoilModeling)

### Author: JMZ
### Modified: 3/17/19
### Purpose: calculate Q10 proportion activities for different plots and sites based on enzyme data

inputFile = 'data-raw/MicrobialExoEnzymeActivities_COMASTER_README.csv'  # Data file we will read in to


inputData=read_csv(inputFile)

# Remove site names that we don't want:
siteNames = c('FEF','UMC','NWT','LSP','SPR')

# Gather all the data together and determine the degree used.
enzymes <- inputData %>%
  gather(key=enzyme,value=activity,-(1:10)) %>%
  separate(enzyme,c("enzyme","temperature"),sep="_") %>%  # Remove the underscore and have column for temperature
  filter(SITE %in% siteNames) %>%
  separate(temperature,c("temperature","junk"),sep="C") %>%  # Remove the C label
  mutate(temperature=as.numeric(temperature)) %>%  # make temperature numeric
  select(-junk) %>% # remove the junk column
  left_join(select(rapid_data,PLOTID,beetles,fire),by="PLOTID")  # Add in treatment codes


# Add in a column to flux_data for the different cases of beetle and fire data, which we call treatments
treatment_key <- expand(enzymes, nesting(beetles, fire)) %>%
  mutate(treatment = 1:n())

# Determine the total activity we have for each sample and temperature
total_activity <- enzymes %>%
  group_by(GalleryNumber,temperature) %>%
  summarize(total_activity = sum(activity))


# Join the treatment data to the enzyme
enzymes %>% inner_join(treatment_key,by=c("beetles","fire")) -> enzymes


# Join the activity data to the enzymes
enzyme_proportion <- enzymes %>%
  inner_join(total_activity,by=c("GalleryNumber","temperature")) %>%
  mutate(proportion = activity/total_activity)


use_data(enzyme_proportion,overwrite = TRUE) # Save all the regression data




# Next: we weight by proportional activity AND enzyme to get a Q10 as a function of temperature for each site.

# The average proportion of enzyme activity weights the contribution to Q10 at a given temperature at each site ...

# Let's think about this:
# - we can calculate the proportion of enzyme activity at each reference temperature.
# - do a regression for the activity weighted Q10 at each reference temperature for each site.


enzymes_Q10 <- enzymes %>%
  group_by(GalleryNumber,enzyme) %>%
  spread(key=temperature,value=activity) %>%
  mutate(Q10_4C = `15`/`4`,Q10_15C = `25`/`15`,Q10_25C = `35`/`25`) %>%
  ungroup() %>%
  select(GalleryNumber,SITE,PLOTID,enzyme,Q10_4C,Q10_15C,Q10_25C) %>% # Pull out variables we need
  gather(key=temperature,value=Q10_ratio,-(1:4)) %>%
  separate(temperature,c("junk","temperature"),sep="_") %>%  # Remove the _ label
  separate(temperature,c("temperature","junk"),sep="C") %>%  # Remove the C label
  mutate(temperature=as.numeric(temperature)) %>%  # make temperature numeric
  select(-junk)  # remove the junk column



 # enzymes_Q10 %>%
  #  ggplot() + geom_line(aes(x=temperature,y=Q10_ratio,group=GalleryNumber),color='grey') +
  #  facet_grid(SITE~enzyme) + ylim(c(0,20)) +
  #  geom_abline(data=hist_data,aes(slope=slope,intercept = intercept),color='blue',size=1)

# We need to join up



# Weight the Q10 ratio by each proportion at each sample, tagging it to the PLOTID
  weighted_Q10 <- enzymes_Q10 %>%
    left_join(select(enzyme_proportion,GalleryNumber,enzyme,temperature,proportion,treatment),
              by=c("GalleryNumber","enzyme","temperature")) %>%
    group_by(treatment,GalleryNumber,temperature) %>%
    summarize(Q10=sum(Q10_ratio*proportion,na.rm=TRUE)) %>%
    filter(temperature==15)  %>%
    left_join(distinct(select(enzyme_proportion,GalleryNumber,PLOTID),GalleryNumber,.keep_all=TRUE),by="GalleryNumber") %>% ungroup()
  ### Our data are just shown in here



  use_data(weighted_Q10,overwrite = TRUE) # Save all the regression data






