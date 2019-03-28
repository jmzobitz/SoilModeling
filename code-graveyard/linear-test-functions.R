likelihood_linear <- function(model_values,data_values,join_key){
  # set up flags for model


  sumll <-  sum(dnorm(model_values,mean=data_values,sd=2,log=TRUE),na.rm=TRUE)



  return(sumll)

}



linear_test <- function(param) {

  ### Author: JMZ
  ### Purpose: determine the amount of respiration from soil sources according to prescribed models

  # for this dataset we just need the flux data, not microbe data

  # Select out parameters
 slope <- param %>%
    filter(name == "slope") %>%
    select(value) %>%
    as.numeric()

 intercept <- param %>%
    filter(name == "intercept") %>%
    select(value) %>%
    as.numeric()


 y_out <- slope*seq(0,20,length.out=300)+intercept



  return(y_out)

}
