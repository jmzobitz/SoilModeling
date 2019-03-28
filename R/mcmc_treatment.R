
#' Does a MCMC estimate across different data treatements for a given model
#'
#' \code{mcmc_treatment} does the heavy lifting for a given model, splitting the data into treatments, runs and saves an MCMC parameter estimate for a given mode.
#'
#' @param model_id The name of the model we are using.  Must be the same name as
#' @param mcmc_mode The type of MCMC estimate we want to run
#' @param treatment_start Where we are starting our treatment.  Helpful if we restart and stop runs.
#'
#' @return Nothing - this just saves a bunch of files


#' @import dplyr
#' @import purrr
#' @export
#'
#'

mcmc_treatment <- function(model_id,mcmc_mode,treatment_start=1) {
  # Spilt off the flux data by treatment
  flux_data_treatment <- flux_data %>%
    filter(treatment>=treatment_start) %>%
    split(.$treatment)  # We've split out the data by treatment

  param_file_name <- paste0('param-defaults/',model_id,'-param.csv')


  # Generate a list of log files
  log_files <-  list(
    file_loc = "mcmc-log-files/",
    id = paste0(model_id,"-"),
    treatment = unique(flux_data$treatment),
    ending=".out"
  ) %>%
    cross_df() %>%
    mutate(file_name = paste0(file_loc,id,treatment,ending)) %>%
    select(file_name,treatment) %>%
    split(.$treatment)

  # Generate a list of result files
  results_files <-  list(
    file_loc = "mcmc-results/",
    id = paste0(model_id,"-"),
    treatment = unique(flux_data$treatment),
    ending=".Rda"
  ) %>%
    cross_df() %>%
    mutate(file_name = paste0(file_loc,id,treatment,ending)) %>%
    select(file_name,treatment) %>%
    split(.$treatment)

  # Specify the model_function
  eval(parse(text=(paste0("model_fn<-",model_id))))

  # Now let's map these out - fingers crossed!
  pmap(list(x=log_files,y=results_files,z=flux_data_treatment),
       .f=function(x,y,z){
         run_MCMC(param_file_name,model_fn,z,"PLOTID",y$file_name,x$file_name,mcmc_mode)
         print(paste0("Ending the treatment run for: ", z$treatment[[1]]))
         })



}


