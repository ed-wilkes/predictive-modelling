#' @name summariseModel
#' @author Ed Wilkes
#' 
#' @description Uses the output of performNestedCV and create summary of performance
#' 
#' @param model model returned from using performNestedCV
#' @param type string denoting type of model (either "binary" or "multi")
#'           
#' @return print of results
#'
summariseModel <- function(model, type) {
  
  if (type == "binary") {
    metric <- "outer_mean_auc"
  } else if (type == "multi") {
    metric <- "outer_mean_balanced_acc"
  }
  
  mean <- round(mean(model$resample[[metric]]), 3)
  sd <- round(sd(model$resample[[metric]]), 3)
  number <- length(model$resample[[metric]])
  
  lower <- round(mean - ((sd/sqrt(number)) * 1.96), 3)
  upper <- round(mean + ((sd/sqrt(number)) * 1.96), 3)
  
  cat(paste0("Mean = ", mean
            ,"\nLower CI = ", lower
            ,"\nUpper CI = ", upper))
  
}