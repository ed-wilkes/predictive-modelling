#' @name plotROC
#' @author Ed Wilkes
#' 
#' @description Uses the output of performNestedCV to plot ROC curves for each fold and 
#' repeat within a nested CV run.
#' 
#' @param data 'predictions' object from performNestedCV output
#' @param target string denoting the target, positive class
#' @param y_measure string denoting the measure to plot on the y-axis
#' @param x_measure string denoting the measure to plot on the x-axis
#' @param order Vector specifying the order of the outcome factor, where the first element
#' is the name negative class and the second is the name of the positive class
#'           
#' @return plot object of ROC curves
#'
plotROC <- function(data, target, y_measure = "sens", x_measure = "fpr", order) {
  
  ## Required packages
  require(ROCR)
  require(dplyr)
  
  ## Pred string
  pred_str <- paste0("pred.", target)
  
  ## Create first ROC plot
  perf <- ROCR::prediction(labels = data[[1]]$obs
                           ,predictions = data[[1]][[pred_str]]
                           ,label.ordering = order) %>% 
    ROCR::performance(measure = y_measure, x.measure = x_measure) 
  plot(perf)
  
  ## Loop through elements of list and add ROC lines
  for(i in 2:length(data)) {
    
    perf <- ROCR::prediction(labels = data[[i]]$obs
                             ,predictions = data[[i]][[pred_str]]
                             ,label.ordering = order) %>%
      ROCR::performance(measure = y_measure, x.measure = x_measure)
    lines(x = perf@x.values[[1]], y = perf@y.values[[1]])
    
  }
  
  ## Collate all data for "average" line
  df_all <- dplyr::bind_rows(data)
  
  perf_all <- ROCR::prediction(labels = df_all$obs
                               ,predictions = df_all[[pred_str]]
                               ,label.ordering = order) %>%
    ROCR::performance(measure = y_measure, x.measure = x_measure)
  lines(x = perf_all@x.values[[1]], y = perf_all@y.values[[1]], col = "red2", lwd = 2)
  abline(a = 0, b = 1)
  
}