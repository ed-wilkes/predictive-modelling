#' @name findProbCutoff
#' @author Ed
#' 
#' @description Finds best class probability to maximise the Youden index of a set of ROC 
#' curves.
#'
#' @param predictions 'prediction_prob' object from performNestedCVParallel output
#' @param order Vector specifying the order of the outcome factor, where the first element
#' is the name negative class and the second is the name of the positive class
#'
#' @return Data frame of the tuned probability thresholds and corresponding performance 
#' statistics
#' 
findProbCutoff <- function(predictions, order, measure_1, measure_2, optimise) {
  
  ## Required packages
  require(ROCR)
  require(dplyr)
  
  ## Create vectors to collect best stats from each fold/repeat
  best_prob <- c()
  best_measure_1 <- c()
  best_measure_2 <- c()
  
  data <- split(predictions, predictions$fold)
  
  ## Loop across each element of the input list (could use lapply() here to speed up)
  for(rep in 1:length(data)) {
    
    perf <- ROCR::prediction(labels = data[[rep]]$obs
                             ,predictions = data[[rep]]$pred
                             ,label.ordering = order) %>% 
      ROCR::performance(measure = measure_1, x.measure = measure_2) 
    
    perf_sum <- data.frame(prob = seq(0.01, 0.99, 0.01))
    
    # Loop through each probability threshold (could use apply() here)
    for(i in 1:dim(perf_sum)[1]) {
      
      threshold <- perf_sum$prob[i]
      thresh_num <- which.min(abs(perf@alpha.values[[1]] - threshold))
      y_measure <-perf@y.values[[1]][thresh_num]
      x_measure <-perf@x.values[[1]][thresh_num]
      perf_sum[[measure_1]][i] <- y_measure
      perf_sum[[measure_2]][i] <- x_measure
      perf_sum[["difference"]][i] <- y_measure - x_measure
      perf_sum[["youden"]][i] <- y_measure + x_measure - 1
      perf_sum[["F1"]][i] <- 2*((y_measure * x_measure)/(y_measure + x_measure))
      perf_sum[["F2"]][i] <- 5*((y_measure * x_measure)/(4*y_measure + x_measure))
      perf_sum[["F4"]][i] <- 17*((y_measure * x_measure)/(16*y_measure + x_measure))
      
    }
    
    if (optimise == "youden") {
      
      best_prob[rep] <- perf_sum$prob[which.min(perf_sum$youden)] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.min(perf_sum$youden)]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.min(perf_sum$youden)]
      
    } else if (optimise == "difference") {
      
      best_prob[rep] <- perf_sum$prob[which.min(abs(perf_sum$difference))] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.min(abs(perf_sum$difference))]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.min(abs(perf_sum$difference))]
      
    } else if (optimise == "F1") {
      
      best_prob[rep] <- perf_sum$prob[which.max(perf_sum$F1)] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.max(perf_sum$F1)]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.max(perf_sum$F1)]
      
    } else if (optimise == "F2") {
      
      best_prob[rep] <- perf_sum$prob[which.max(perf_sum$F2)] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.max(perf_sum$F2)]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.max(perf_sum$F2)]
      
    } else if (optimise == "F4") {
      
      best_prob[rep] <- perf_sum$prob[which.max(perf_sum$F4)] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.max(perf_sum$F4)]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.max(perf_sum$F4)]
      
      
    } else if (optimise == measure_1) {
      
      best_prob[rep] <- perf_sum$prob[which.max(perf_sum[[measure_1]])] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.max(perf_sum[[measure_1]])]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.max(perf_sum[[measure_1]])]
      
    } else if (optimise == measure_2) {
      
      best_prob[rep] <- perf_sum$prob[which.max(perf_sum[[measure_2]])] 
      best_measure_1[rep] <- perf_sum[[measure_1]][which.max(perf_sum[[measure_2]])]
      best_measure_2[rep] <- perf_sum[[measure_2]][which.max(perf_sum[[measure_2]])]
      
    }
    
  }
  
  df_best_probs <- data.frame(fold = names(data)
                              ,best_prob
                              ,best_measure_1
                              ,best_measure_2)
  return(df_best_probs)
  
}
