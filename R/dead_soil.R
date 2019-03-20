#' Compute the dead_soil version of a model
#'
#' \code{dead_soil} Returns the prior probability of a parameter set
#'
#' @param param input parameter vector - is a data frame with the following columns:
#' 1. name
#' 2. changeable
#' 3. value
#' 4. minVal
#' 5. maxVal
#'
#' @return rSoil at each of the Plots with a given value


#' @import dplyr
#' @importFrom plyr ldply
#' @import purrr
#' @export
#'
#'

dead_soil <- function(param) {

  ### Author: JMZ
  ### Purpose: determine the amount of respiration from soil sources according to prescribed models

  # for this dataset we just need the flux data, not microbe data

  # Select out parameters
  soilRespMoistEffect <- param %>%
    filter(name == "soilRespMoistEffect") %>%
    select(value) %>%
    as.numeric()

   baseResp <- param %>%
    filter(name == "baseSoilResp") %>%
    select(value) %>%
    as.numeric()


  # Pull in the data that we need to do this problem
  data <- flux_data %>% right_join(select(microbe_data,PLOTID,soilC,soilWater),by="PLOTID")

  # Compute the reduction in R due to water effects
  waterEffect <- data %>%
    transmute(PLOTID,moistEffect=(soilWater/100)^soilRespMoistEffect)

  # Now compute the Q10 value at each site, using the regressions we found
  regressions <- Q10_temperature %>% filter(site %in% unique(data$site)) %>% split(.$site)
  temperature <- data %>% select(site,PLOTID,Tsoil_C) %>% split(.$site)
  Q10Effect <- map2(regressions,temperature,
                     ~(data.frame(PLOTID=.y$PLOTID,tempEffect=(.x$intercept+.x$slope*.y$Tsoil_C)^(.y$Tsoil_C/10))
                       )
                     )  %>%
    ldply(data.frame,.id="site") %>%  # Join them all up into a data frame
    mutate(PLOTID=as.character(PLOTID))


    # Now join the two effects together
    effects <- Q10Effect %>%
      inner_join(waterEffect,by="PLOTID") %>%
      inner_join(select(data,PLOTID,soilC),by="PLOTID")

    # Compute rSoil
    rSoil <- effects %>% transmute(site,PLOTID,value=baseResp*soilC*moistEffect*tempEffect)


  return(rSoil)

}
