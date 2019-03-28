#' Compute the loglikelihood of a model and data version of a model
#'
#' \code{likelihood} Returns the likelihood of a modeled value
#'
#' @param model_values input model_values - a dataframe with (at least) two columns:
#' 1. value: the modeled flux value
#' 2. join_key --> This allows us to connect it to the measured data
#' @param join_key The name of the variable we will join to flux_data
#'
#' @return logll of the measured an modeled data


#' @import dplyr
#' @export
#'
#'


likelihood <- function(model_values,data_in){
  # set up flags for model


  sumll <- sum(dnorm(model_values$value,mean=data_in$rSoil,sd=data_in$rSoilErr,log=TRUE),na.rm=TRUE)



  return(sumll)

}
