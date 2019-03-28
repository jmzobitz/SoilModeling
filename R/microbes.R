#' Compute the dead_soil version of a model
#'
#' \code{microbes} Returns the prior probability of a parameter set
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
#' @export
#'
#'

microbes <- function(param,data_in) {

  ### Author: JMZ
  ### Purpose: determine the amount of respiration from soil sources according to prescribed models

  # for this dataset we just need the flux data, not microbe data

  # Select out parameters
  soilRespMoistEffect <- param %>%
    filter(name == "soilRespMoistEffect") %>%
    pull(value)

  baseResp <- param %>%
    filter(name == "baseMicrobeResp") %>%
    pull(value)

  efficiency <- param %>%
    filter(name == "efficiency") %>%
    pull(value)

  maxIngestionRate <- param %>%
    filter(name == "maxIngestionRate") %>%
    pull(value)

  halfSatIngestion <- param %>%
    filter(name == "halfSatIngestion") %>%
    pull(value)

  # Compute the reduction in R due to water effects
  moistEffect <- (data_in$soilWater/100)^soilRespMoistEffect

  soilC <- data_in$soilC
  microbeC <- data_in$microbeC
  #
  growthRespiration <- (1-efficiency)*maxIngestionRate*(soilC)/(halfSatIngestion+soilC)*microbeC
  heteroRespiration <- baseResp*microbeC*data_in$tempEffect*moistEffect



  # Compute rSoil
  rSoil <- data_in %>% transmute(site,PLOTID,value=growthRespiration+heteroRespiration)


  return(rSoil)

}


