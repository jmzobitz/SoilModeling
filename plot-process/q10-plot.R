library(tidyverse)
library(devtools)
library(SoilModeling)

### Author: JMZ
### Modified: 3/17/19
### Purpose: plot Q10 activity


### Make a plot of the weighted Q10 by treatment
q10plot <- weighted_Q10 %>%
  ggplot(aes(x = treatment, y = Q10,color=as.factor(treatment))) +
  geom_jitter(size=3) +
  geom_boxplot(outlier.size=0,alpha=0.5) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(x='Treatment',y = bquote(''*Q[10]*'')) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold")) +
  guides(color=FALSE)

# Make a plot of the proportion of enzyme activity at each reference temperature and enzyme
fileName <- paste0('manuscript-figures/q10EnzymeSummary.png')
ggsave(fileName,plot=q10plot,width=7,height=5)







