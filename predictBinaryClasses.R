#' @name predictBinaryClasses
#' @author Ed Wilkes
#' 
#' @description Takes tuned thresholds from findProbCutoff and makes hard class calls on 
#' outputs from performedNestedCVParallel
#' 
#' @param predictions 'predictions' object from performNestedCVParallel output
#' @param thresholds Threshold data frame from findProbCutoff output
#' @param target String denoting the target, positive class
#'
#' @return List of data frames containing the predictions made using the tuned probability
#' thresholds from findProbCutoff
#'
predictBinaryClasses <- function(predictions, thresholds, target) {
  
  ## Split input data frame into separate lists for each fold
  predictions <- split(predictions, predictions$fold)
  
  ## Find names of positive and negative classes
  label_vector <- unique(predictions[[1]]$obs)
  if(length(label_vector) > 2) {
    stop("There are more than two levels in the outcome variable, binary classification
         is not possible!")
  }
  neg_class <- as.character(label_vector[which(label_vector != target)])
  pos_class <- as.character(label_vector[which(label_vector == target)])
  
  ## Loop across predictions and make hard prediction based on tuned threshold
  for(rep in 1:length(predictions)) {
    
    threshold <- thresholds$best_prob[which(thresholds$fold == names(predictions)[rep])]
    
    # Predict class based on probability threshold
    colnames(predictions[[rep]])[which(colnames(predictions[[rep]]) == "pred")] <- "pred_num"
    predictions[[rep]]$pred <- factor(ifelse(predictions[[rep]]$pred_num >= threshold
                                             ,yes = pos_class
                                             ,no = neg_class)
                                      ,levels = levels(predictions[[rep]]$obs))
    
  }
  
  return(bind_rows(predictions))
  
}