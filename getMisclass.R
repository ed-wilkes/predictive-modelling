#' @name getMisclass
#' @author Ed
#' 
#' @description Uses hard class predictions from predictBinaryClasses to sum the number of
#' times each ID in the original data is misclassified.
#' 
#' @param data Data frame containing original modelling data
#' @param predictions List from predictBinaryClasses output
#' @param thresholds Data frame from findProbCutoff output
#' @param ylabel String denoting the name of the outcome variable
#' @param target String denoting the target, positive class
#'
#' @return Data frame containing list of IDs in the data with a sum of how many times they were misclassified
#' 
getMisclass <- function(data, predictions, thresholds, ylabel, target) {
  
  ## Define predict string
  pred_str <- paste0("pred.", target)
  list_output <- list()
  
  ## Find names of positive and negative classes
  label_vector <- unique(data[[ylabel]])
  if(length(label_vector) > 2) {
    stop("There are more than two levels in the outcome variable, binary classification
         is not possible!")
  }
  neg_class <- as.character(label_vector[which(label_vector != target)])
  pos_class <- as.character(label_vector[which(label_vector == target)])

  ## Loop across predictions and get IDs of misclassified samples
  for(rep in 1:length(predictions)) {
    
    threshold <- thresholds$best_prob[rep]
    
    # Predict class based on probability threshold
    predictions[[rep]]$pred <- ifelse(predictions[[rep]][pred_str] >= threshold
                                      ,yes = pos_class
                                      ,no = neg_class) %>% as.factor
    
    # Determine if prediction matches observed value
    predictions[[rep]]$misclass <- ifelse(predictions[[rep]]$pred != 
                                            predictions[[rep]]$obs
                                          ,yes = "yes"
                                          ,no = "no") %>% as.factor
    
    # Pull ID numbers of samples
    predictions[[rep]]$ID <- data$ID[predictions[[rep]]$row_num]
    
    # Filter data for misclassified samples
    df_output <- dplyr::filter(predictions[[rep]], misclass == "yes")
    list_output[[rep]] <- df_output
    
  }
  
  df_all <- dplyr::bind_rows(list_output) %>%
     dplyr::group_by(ID, obs) %>%
     dplyr::summarise(times_misclassified = n()) %>%
     dplyr::arrange(obs, -times_misclassified)
  return(df_all)
  
}