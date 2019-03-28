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
#' @export
#'
#'

dead_soil <- function(param,data_in) {

  ### Author: JMZ
  ### Purpose: determine the amount of respiration from soil sources according to prescribed models

  # for this dataset we just need the flux data, not microbe data

  # Select out parameters
  soilRespMoistEffect <- param %>%
    filter(name == "soilRespMoistEffect") %>%
    pull(value)

   baseResp <- param %>%
    filter(name == "baseSoilResp") %>%
    pull(value)


  # Compute the reduction in R due to water effects
  moistEffect <- (data_in$soilWater/100)^soilRespMoistEffect

    # Compute rSoil
    rSoil <- data_in %>% transmute(site,PLOTID,value=baseResp*soilC*moistEffect*tempEffect)


  return(rSoil)

}
