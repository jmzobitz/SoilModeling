#' Return a random sample of a parameter set
#'
#' \code{sample_parameters} Returns a parameter set where a given parameter has been changed for a parameter set
#'
#' @param param input parameter vector - is a data frame with the following columns:
#' 1. name
#' 2. changeable
#' 3. value
#' 4. minVal
#' 5. knob
#' 6. sampled
#'
#' @return updated parameter values, with a "knob" column added for simulating the temperatures.
#'
#' @import dplyr
#' @export



sample_parameters <- function(param)	{

  tune <- runif(1,min=-0.5,max=0.5)

  # Pick out a row to change
  sampled_param <- param %>%
    filter(changeable) %>%
    sample_n(1) %>%
    mutate(value = knob*(maxVal-minVal)+tune*value,
           sampled=TRUE) %>%
    select(name,value,sampled)

  # Now join the sampled parameter to the current list, updating accordingly

  out_param <- param %>%
    mutate(value=if_else(name==sampled_param$name,sampled_param$value,value)) %>%
    mutate(sampled=if_else(name==sampled_param$name,TRUE,FALSE))

  return(out_param)
}

