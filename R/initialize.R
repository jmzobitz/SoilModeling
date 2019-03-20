#' Return a random initial start of a parameter set
#'
#' \code{initialize} Returns a random start value for a parameter set
#'
#' @param param input parameter vector - is a data frame with the following columns:
#' 1. name
#' 2. changeable
#' 3. value
#' 4. minVal
#' 5.
#'
#' @return updated parameter values, with a "knob" column added for simulating the temperatures.
#'
#' @import dplyr
#' @export


### Random initial start
initialize <- function(param)	{

  updatedParam <- param %>%
    mutate(value=if_else(changeable==1,runif(n(),min=minVal,max=maxVal),value)) %>%
    mutate(knob=1) %>% # add in a row of "temperatures" for simulated annealing
    mutate(sampled=FALSE)  # a T/F to see if this has been sampled or not.


  return(updatedParam)
}

