library(tidyverse)


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
  select(-junk)  # remove the junk column


# Determine the total activity we have for each sample and temperature
total_activity <- enzymes %>%
  group_by(GalleryNumber,temperature) %>%
  summarize(total_activity = sum(activity))

# Join the activity data to the enzymes

enzyme_proportion <- enzymes %>%
  inner_join(total_activity,by=c("GalleryNumber","temperature")) %>%
  mutate(proportion = activity/total_activity) #%>%
 # filter(temperature != "35C")  # remove the high temperature activity



enzyme_plot <- enzyme_proportion %>%
  ggplot(aes(x = factor(temperature), y = proportion)) +
  geom_jitter(size=1,aes(color=factor(temperature))) +
  geom_boxplot(outlier.size=0,alpha=0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_grid(.~ enzyme) +
  labs(x='Temperature (degrees Celsius)',y = 'Proportional Activity') +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"),strip.background = element_rect(colour="white", fill="white"))+ scale_fill_discrete(name="Site")+
  guides(color=FALSE)


# Make a plot of the proportion of enzyme activity at each reference temperature and enzyme
fileName <- paste0('manuscript-figures/q10ProportionEnzymeSummary.png')
ggsave(fileName,plot=enzyme_plot,width=12,height=5)


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
  select(GalleryNumber,SITE,enzyme,Q10_4C,Q10_15C,Q10_25C) %>% # Pull out variables we need
  gather(key=temperature,value=Q10_ratio,-(1:3)) %>%
  separate(temperature,c("junk","temperature"),sep="_") %>%  # Remove the _ label
  separate(temperature,c("temperature","junk"),sep="C") %>%  # Remove the C label
  mutate(temperature=as.numeric(temperature)) %>%  # make temperature numeric
  select(-junk)  # remove the junk column



  #enzymes_Q10 %>%
  #  ggplot() + geom_line(aes(x=temperature,y=Q10_ratio,group=GalleryNumber),color='grey') +
  #  facet_grid(SITE~enzyme) + ylim(c(0,20)) +
  #  geom_abline(data=hist_data,aes(slope=slope,intercept = intercept),color='blue',size=1)


# Weight the Q10 ratio by each proportion at each sample
  weighted_Q10 <- enzymes_Q10 %>%
    left_join(select(enzyme_proportion,GalleryNumber,enzyme,temperature,proportion,SITE),
              by=c("GalleryNumber","enzyme","temperature","SITE")) %>%
    group_by(SITE,GalleryNumber,temperature) %>%
    summarize(Q10=sum(Q10_ratio*proportion,na.rm=TRUE))

  # Define a general fitting function
  fit_enzyme_weighted <- function(sample) {

    fit_data <- sample %>% split(.$GalleryNumber) %>%
      map(~lm(.x$Q10 ~.x$temperature)) %>%
      map(summary) %>%
      map(broom::tidy) %>%
      bind_rows(.id="GalleryNumber")

    return(fit_data)

  }

  # Now we can plot the histogram by slope and intercept
  split_data_weighted <- weighted_Q10 %>% split(.$SITE) %>%
    map(fit_enzyme_weighted) %>%
    bind_rows(.id="SITE")

  # We have this almost ...
  # YAY!  Now do a histogram across all the sites
  Q10_temperature <- split_data_weighted %>%
    group_by(SITE,term) %>%
    summarize(median=median(estimate) #,
              # q025=quantile(estimate,0.025),
              #  q975=quantile(estimate,0.975)
    ) %>%
    spread(key=term,value=median) %>%
    rename(slope=2,intercept=3,site=SITE)


use_data(Q10_temperature,overwrite = TRUE)

  ## Let's make a plot of this Q10 as a function of temperature
q10plot <-  weighted_Q10 %>%
    ggplot() + geom_line(aes(x=temperature,y=Q10,group=GalleryNumber),color='grey') +
    facet_grid(.~SITE) +
    geom_abline(data=Q10_temperature,aes(slope=slope,intercept=intercept),color="blue",size=1) +
    ylim(c(0,15)) +
    labs(x ='Temperature (degrees Celsius)', y = bquote(''*Q[10]*'')) +
    theme_bw(base_size = 16, base_family = "Helvetica") +
    theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"),strip.background = element_rect(colour="white", fill="white"))+ scale_fill_discrete(name="Site")+
    guides(color=FALSE)



# Make a plot of the proportion of enzyme activity at each reference temperature and enzyme
fileName <- paste0('manuscript-figures/q10EnzymeSummary.png')
ggsave(fileName,plot=q10plot,width=12,height=5)

