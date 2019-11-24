library(tidyverse)
library(devtools)
library(SoilModeling)

### Author: JMZ
### Modified: 3/17/19
### Purpose: plot Q10 activity

site_names <- c("UMC","SPR","LSP")

# Add the site names
my_q10 <- weighted_Q10 %>%
  mutate(site=str_sub(PLOTID,1,3)) %>%
  filter(site %in% site_names)

### Make a plot of the weighted Q10 by treatment
q10plot <- my_q10 %>%
  ggplot(aes(x = treatment, y = Q10,color=as.factor(treatment))) +
  geom_jitter(size=3) +
  geom_boxplot(outlier.size=0,alpha=0.5) +
  #coord_cartesian(ylim = c(0, 7)) +
  facet_grid(.~site) +
  labs(x='Treatment',y = bquote(''*Q[10]*'')) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x=element_text(face="bold"),axis.title.y=element_text(face="bold"),strip.background = element_rect(colour="white", fill="white"))+
  guides(color=FALSE)


# Report out the AOV (for methods)  (HELPFUL PAGE: http://personality-project.org/r/r.guide/r.anova.html#oneway)
summary(aov(Q10~treatment*site,data=my_q10))


# Make a plot of the proportion of enzyme activity at each reference temperature and enzyme
fileName <- paste0('manuscript-figures/q10EnzymeSummary.png')
ggsave(fileName,plot=q10plot,width=12,height=4)







