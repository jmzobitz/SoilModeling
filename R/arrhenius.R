#' Compute the arrhenius version of a soil respiration
#'
#' \code{arrhenius} returns the predicted soil respiration vlaues
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



arrhenius <- function(param,data_in) {

  # Select out parameters
  soilRespMoistEffect <- param %>%
    filter(name == "soilRespMoistEffect") %>%
    pull(value)

  baseResp <- param %>%
    filter(name == "baseSoilResp") %>%
    pull(value)

  activationEnergy <- param %>%
    filter(name == "activationEnergy") %>%
    pull(value)


  # Compute the reduction in R due to water effects
  moistEffect <- (data_in$soilWater/100)^soilRespMoistEffect


  # Compute the reduction in R due to temperature
  #  3 =  k * exp(-a / (8.3144598 * 290) 5/283.15) * 0.03
  rGas <- 8.3144598   # J / K / mol
  tempEffect <-  exp( -activationEnergy / (rGas*(data_in$T_soilC+273.15)))


  # Compute rSoil
  rSoil <- data_in %>% transmute(site,PLOTID,value=baseResp*soilC*moistEffect*tempEffect)

  return(rSoil)



}
