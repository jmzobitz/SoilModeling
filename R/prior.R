#' Return the prior value of a parameter set
#'
#' \code{prior} Returns the prior probability of a parameter set
#'
#' @param param input parameter vector - is a data frame with the following columns:
#' 1. name
#' 2. changeable
#' 3. value
#' 4. minVal
#' 5.
#'
#' @return Prior probability density


#' @import dplyr
#' @export
#'
#'
prior <- function(param)	{


  priorProb <- param %>%
    filter(changeable) %>%
    summarize(probability = sum(dunif(value,min=minVal,max=maxVal,log=TRUE))) %>%
    as.numeric()


  return(priorProb)
}
