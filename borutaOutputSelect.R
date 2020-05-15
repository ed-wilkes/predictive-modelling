#' @name borutaOutputSelect
#' @author Ed Wilkes
#' 
#' @description finds features >= shadowMax median and extracts these from input data 
#' 
#' @param data Data frame containing data to model
#' @param y String denoting the name of the outcome variable in the input data
#' @param boruta_obj List output from borutaCVParallel()
#' 
#' @return Data frame containing selected features
#'
borutaOutputSelect <- function(data
                               ,id_var
                               ,y
                               ,boruta_obj) {
  
  ## Required packages
  require(dplyr)
  
  ## Get features from Boruta object, calculate median importance, and filter input data
  df_features <- dplyr::bind_rows(boruta_obj$importance) %>%
    filter(!is.infinite(value)) %>%
    group_by(variable) %>%
    summarise(median = median(value)) %>%
    arrange(-median)
  shadow_max <- filter(df_features, variable == "shadowMax")$median
  selected_features <- filter(df_features, median > shadow_max)$variable
  df_output <- select(data, c(id_var, all_of(selected_features), y))
  return(df_output)
  
}