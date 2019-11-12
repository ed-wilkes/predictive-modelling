#' @name multiClassSummaryF4
#' @author topepo (edited by Ed Wilkes)
#'
#' @description Edited version of topepo's caret::multiClassSummary which computes the F4
#' score in place of the F1 score for multi-class models
#' 
multiClassSummaryF4 <- function (data, lev = NULL, model = NULL) 
{
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  has_class_probs <- all(lev %in% colnames(data))
  if (has_class_probs) {
    lloss <- caret::mnLogLoss(data = data, lev = lev, model = model)
    caret:::requireNamespaceQuietStop("ModelMetrics")
    caret:::requireNamespaceQuietStop("MLmetrics")
    prob_stats <- lapply(levels(data[, "pred"]), function(x) {
      obs <- ifelse(data[, "obs"] == x, 1, 0)
      prob <- data[, x]
      roc_auc <- try(ModelMetrics::auc(obs, data[, x]), 
                     silent = TRUE)
      pr_auc <- try(MLmetrics::PRAUC(y_pred = data[, x], 
                                     y_true = obs), silent = TRUE)
      if (inherits(pr_auc, "try-error")) 
        pr_auc <- NA
      res <- c(ROC = roc_auc, AUC = pr_auc)
      return(res)
    })
    prob_stats <- do.call("rbind", prob_stats)
    prob_stats <- colMeans(prob_stats, na.rm = TRUE)
  }
  CM <-  caret::confusionMatrix(data[, "pred"], data[, "obs"], 
                        mode = "everything")
  CM$byClass[,7] <- 17*((CM$byClass[,5] * CM$byClass[,6])/(16*CM$byClass[,5] + CM$byClass[,6])) # convert F1 to F2 score
  
  if (length(levels(data[, "pred"])) == 2) {
    class_stats <- CM$byClass
  }
  else {
    class_stats <- colMeans(CM$byClass, na.rm = TRUE)
    names(class_stats)[7] <- "F4"
    names(class_stats) <- paste("Mean", names(class_stats))
  }
  overall_stats <- if (has_class_probs) 
    c(CM$overall, logLoss = as.numeric(lloss), AUC = unname(prob_stats["ROC"]), 
      prAUC = unname(prob_stats["AUC"]))
  else CM$overall
  stats <- c(overall_stats, class_stats)
  stats <- stats[!names(stats) %in% c("AccuracyNull", 
                                      "AccuracyLower", "AccuracyUpper", "AccuracyPValue", 
                                      "McnemarPValue", "Mean Prevalence", "Mean Detection Prevalence")]
  names(stats) <- gsub("[[:blank:]]+", "_", names(stats))
  stat_list <- c("Accuracy", "Kappa", "Mean_F4", 
                 "Mean_Sensitivity", "Mean_Specificity", "Mean_Pos_Pred_Value", 
                 "Mean_Neg_Pred_Value", "Mean_Precision", 
                 "Mean_Recall", "Mean_Detection_Rate", "Mean_Balanced_Accuracy")
  if (has_class_probs) 
    stat_list <- c("logLoss", "AUC", "prAUC", 
                   stat_list)
  if (length(levels(data[, "pred"])) == 2) 
    stat_list <- gsub("^Mean_", "", stat_list)
  stats <- stats[c(stat_list)]
  return(stats)
}