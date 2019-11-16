##  Taylor Plot for modis reflectance - Figure 6
##########
library(tidyverse)
# modisBRDF weights
# we are taking modisBRDF and the site lists - maybe left join by site and time, selecting out the kernel - arrgh, this will be a little tricky, but we should be ok.

load('mcmc-results/modeled-rSoil-results.Rda')

#modeled_rSoil %>% filter(model_id=='dead_soil') %>%
#  ggplot() + geom_point(aes(x=modeled,y=measured,color=site))

#flux_data %>% ggplot() + geom_point(aes(x=soilC,y=rSoil,color=site)) + facet_grid(.~treatment)

taylor_values <- modeled_rSoil %>%
  group_by(type,names,treatment) %>%
  summarize(
  sd_meas = 1,
  sd_model = sd(modeled) / sd(measured),
  r = cor(modeled,measured),
  centered_rms = sd((measured-mean(measured))-((modeled-mean(modeled))))/sd(measured),
  x_coord = sd_model*r,
  y_coord = sd_model*sin(acos(r))
)



# normalize the results, see Taylor 2001
# E = E'/sigma_meas
# sigma_model = sigma_model/sigma_meas
# sigma_meas = 1


t_plot <- taylor_plot()

curr_plot <- t_plot +
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=names,shape=type),size=2) +
  facet_grid(.~treatment) +
  labs(x="",y=expression(italic("\u03C3")[model]),color="Model",shape="Estimate type") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size=14),
        axis.title=element_text(size=28),
        title=element_text(size=26),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12),
        strip.background = element_rect(colour="white", fill="white")) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




fileName <- paste0('manuscript-figures/taylor-plot.png')
ggsave(fileName,plot=curr_plot,width=6,dpi=600)

