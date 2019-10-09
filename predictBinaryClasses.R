#' @name predictBinaryClasses
#' @author Ed
#' 
#' @description Takes thresholds from findProbCutoff and makes hard class calls on outputs
#' from performedNestedCV.
#' 
#' @param predictions 'predictions' object from performNestedCV output
#' @param thresholds Threshold data frame from findProbCutoff output
#' @param target String denoting the target, positive class
#'
#' @return List of data frames containing the predictions made using the tuned probability
#' thresholds from findProbCutoff
#'
predictBinaryClasses <- function(predictions, thresholds, target) {
  
  ## Define predict string
  pred_str <- paste0("pred.", target)
  list_output <- list()
  
  ## Find names of positive and negative classes
  label_vector <- unique(predictions[[1]]$obs)
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
    
  }
  
  return(predictions)
  
}