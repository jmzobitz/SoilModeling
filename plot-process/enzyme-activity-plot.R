library(tidyverse)
library(devtools)
library(SoilModeling)

### Author: JMZ
### Modified: 3/17/19
### Purpose: plot enzyme activity data



enzyme_plot <- enzyme_proportion %>%
  filter(temperature ==15) %>%
  ggplot(aes(x = treatment, y = proportion,color=as.factor(treatment))) +
  geom_jitter(size=1) +
  geom_boxplot(outlier.size=0,alpha=0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_grid(.~enzyme) +
  labs(x='Treatment',y = "Proportional activity at 15 \u00B0C") +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"),strip.background = element_rect(colour="white", fill="white"))+ scale_fill_discrete(name="Site")+
  guides(color=FALSE)


# Make a plot of the proportion of enzyme activity at each reference temperature and enzyme
fileName <- paste0('manuscript-figures/q10ProportionEnzymeSummary.png')
ggsave(fileName,plot=enzyme_plot,width=12,height=5)
