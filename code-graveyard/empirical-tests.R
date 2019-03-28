# empirical plot - scrap work



# Pull in the data that we need to do this problem
data <- flux_data %>% right_join(select(microbe_data,PLOTID,soilC,soilWater),by="PLOTID")

# Dead soil:

# Now compute the Q10 value at each site, using the regressions we found
regressions <- Q10_temperature %>% filter(site %in% unique(data$site)) %>% split(.$site)
temperature <- data %>% select(site,PLOTID,Tsoil_C) %>% split(.$site)
Q10Effect <- map2(regressions,temperature,
                  ~(data.frame(PLOTID=.y$PLOTID,tempEffect=(.x$intercept+.x$slope*.y$Tsoil_C)^(.y$Tsoil_C/10))
                  )
)  %>%
  bind_rows(.id="site")
# Compute the reduction in R due to water effects
waterEffect <- data %>%
  transmute(PLOTID,moistEffect=(soilWater/100))


# Now join the two effects together
effects <- cbind(Q10Effect, waterEffect, select(data,soilC))
                 %>%
  inner_join(waterEffect,by=c("PLOTID")) %>%
  inner_join(select(data,PLOTID,soilC),by="PLOTID")

x <- effects$tempEffect*effects$moistEffect*effects$soilC
y <- data$rSoil
yoop <- lm(y~-1+x)
