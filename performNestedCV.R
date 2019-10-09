#' @name performedNestedCV
#' @author Ed Wilkes
#' 
#' @description performs nested CV, using the inner loop to select the optimal hyper
#' -parameters, and the outer loop to test the optimal model on a test set. 
#' 
#' @param data Data frame containing data to model
#' @param y String denoting the name of the outcome variable in the input data
#' @param method String denoting the machine learning algorithm to use
#' @param metric String denoting the metric to optimise in the inner CV loop
#' @param outer_k Numeric denoting the number of folds in the outer CV loop
#' @param outer_rep Numeric denoting the number of times to repeat the outer CV 
#' @param inner_k Numeric denoting the number of folds in the inner CV loop
#' @param inner_rep Numeric denoting the number of times to repeat the inner CV
#' @param sampling String denoting the sampling strategy to use in the inner CV loop
#' @param seed Numeric giving the random seed to set
#' @param verbose Logical denoting whether to print real-time inner CV loop process to 
#' console
#' @param feat_select Logical denoting whether to perform Boruta feature selection in the
#' inner CV loop
#' @param max_runs Numeric denoting the maximum number of iterations to perform for 
#' Boruta feature selection
#'
#' @return List with the following objects:
#'           (i) Inner and outer CV loop resample results
#'           (ii) Averaged results
#'           (iii) Algorithm used (i.e., "method" argument)
#'           (iv) Sampling method used
#'           (v) Outer CV settings
#'           (vi) Inner CV settings
#'           (vii) Random seed setting
#'           (viii) list of confusion matrices from each outer fold
#'
performNestedCV <- function(data
                            ,y
                            ,method
                            ,metric
                            ,outer_k
                            ,outer_rep
                            ,inner_k
                            ,inner_rep
                            ,sampling = "none"
                            ,seed = 123
                            ,verbose = FALSE
                            ,feat_select = FALSE
                            ,max_runs = 100) {
  
  ## Required packages
  require(dplyr)
  require(caret)
  
  ## Check if "y" is in data ----
  if(!y %in% colnames(data)) {
    stop(paste0("Your outcome variable, '", y, "', is not in the dataset"))
  }
  
  ## Check if "y" is a factor ----
  if(!is.factor(data[[y]])) {
    message("Outcome variable is not a factor, coercing ...")
    data[[y]] <- factor(data[[y]])
  }
  
  ## Check if "verbose" is boolean ----
  if(!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical.")
  }
  
  ## Check levels in outcome vector ----
  if(length(levels(data[[y]])) > 2) {
    message("Outcome vector has more than one level, assuming multiclass analysis ...")
    summary <- "multiClassSummary"
  } else {
    message("Outcome vector has two levels, assuming two class analysis ...")
    summary <- "twoClassSummary"
  }
  
  ## Create results lists ----
  list_results <- list()
  list_pred <- list()
  
  ## Check that folds/reps have been defined, default if not ----
  if(missing(outer_k)) {
    warning("No value given for 'outer_k', defaulting to 2.")
    outer_k <- 2
  }
  if(missing(outer_rep)) {
    warning("No value given for 'outer_rep', defaulting to 5.")
    outer_rep <- 5
  } 
  if(missing(inner_k)) {
    warning("No value for 'inner_k', defaulting to 2.")
    inner_k <- 2
  }
  if(missing(inner_rep)) {
    warning("No value for 'inner_rep', defaulting to 5.")
    inner_rep <- 5
  }
  
  ## Warn if feature selection is requested ----
  if(feat_select == TRUE) {
    message("Feature selection is enabled, this will substantially increase run time!")
  }
  
  ## Check if sampling is correct string ----
  if(!sampling %in% c("none", "down", "smote", "rose")) {
    stop("'sampling' must be a string of value 'none', 'down', 'smote', or 'rose'.")
  }
  
  ## Define inner CV control ----
  if(sampling == "none") {
    
    inner_control <- caret::trainControl(method = "repeatedcv"
                                         ,number = inner_k
                                         ,repeats = inner_rep
                                         ,verboseIter = verbose
                                         ,classProbs = TRUE
                                         ,trim = TRUE
                                         ,summaryFunction = match.fun(summary))
    
  } else if(sampling != "none") {
    
    inner_control <- caret::trainControl(method = "repeatedcv"
                                         ,number = inner_k
                                         ,repeats = inner_rep
                                         ,verboseIter = verbose
                                         ,classProbs = TRUE
                                         ,sampling = sampling
                                         ,trim = TRUE
                                         ,summaryFunction = match.fun(summary))
    
  }
  
  ## Set seed and define outer folds for each repeat
  set.seed(seed)
  list_folds <- caret::createMultiFolds(y = data[[y]], k = outer_k, times = outer_rep)
  if (feat_select == TRUE) {
    list_features <- list()
  }
  
  ## Outer CV repeat loop ----
  for(rep in 1:outer_rep) {
    
    # Initialise results data frame
    if(summary == "twoClassSummary") {
      
      df_results <- data.frame(fold = 1:outer_k
                               ,rep = rep(rep, times = outer_k)
                               ,inner_mean_auc = NA
                               ,outer_mean_sens = NA
                               ,outer_mean_spec = NA
                               ,outer_mean_auc = NA)
      
    } else if(summary == "multiClassSummary") {
      
      df_results <- data.frame(fold = 1:outer_k
                               ,rep = rep(rep, times = outer_k)
                               ,inner_mean_balanced_acc = NA
                               ,outer_mean_sens = NA
                               ,outer_mean_spec = NA
                               ,outer_mean_ppv = NA
                               ,outer_mean_npv = NA
                               ,outer_mean_f1 = NA
                               ,outer_mean_balanced_acc = NA)
      
    }
    
    # Outer CV loop (test set evaluation) ----
    for(k in 1:outer_k) {
      
      # Get row numbers of train data
      if(rep < 10 && outer_rep >= 10) {
        fold_name <- paste0("Fold", k, ".Rep0", rep)
      } else {
        fold_name <- paste0("Fold", k, ".Rep", rep)
      }
      rows_train <- list_folds[[fold_name]]
      
      message(paste0("\nOuter loop: ", fold_name, " processing ..."))
      if(k == 1 && rep == 1) {
        time_start <- Sys.time()
      }
      
      # Create train and test data
      df_train <- data[rows_train,]
      df_test <- data[-rows_train,]
      test_rows <- as.numeric(row.names(df_test))
      
      # Select features if required
      if(feat_select == TRUE) {
        
        message("Performing feature selection ...")
        model <- Boruta::Boruta(as.formula(paste(paste(y),"~ ."))
                                ,df_train
                                ,doTrace = 0
                                ,maxRuns = max_runs) %>%
          Boruta::TentativeRoughFix()
        
        vars_boruta <- c(Boruta::getSelectedAttributes(model
                                                       ,withTentative = FALSE)
                         ,y)
        
        df_train <- df_train[, which(colnames(df_train) %in% vars_boruta)]
        list_features[[fold_name]] <- vars_boruta
        
      }
      
      # Set seed again to ensure reproducible inner loop fold selection
      set.seed(seed)
      
      # Inner CV loop (for hyperparameter tuning) defined by trControl argument
      inner_model <- caret::train(form = as.formula(paste(paste(y),"~ ."))
                                  ,data = df_train
                                  ,method = method
                                  ,metric = metric
                                  ,trControl = inner_control)
      
      cat("Inner loop tune results: ")
      if(summary == "twoClassSummary") {
        
        cat(paste0("ROC = ", round(getTrainPerf(inner_model)[["TrainROC"]], 4)))
        df_results$inner_mean_auc[k] <- 
          round(getTrainPerf(inner_model)[["TrainROC"]], 4) %>% as.numeric
        
      } else if(summary == "multiClassSummary") {
        
        cat(paste0("BA = "
                   ,round(getTrainPerf(inner_model)[["TrainMean_Balanced_Accuracy"]], 4)))
        df_results$inner_mean_balanced_acc[k] <- 
          round(getTrainPerf(inner_model)[["TrainMean_Balanced_Accuracy"]], 4) %>% 
          as.numeric
        
      }
      
      # Get predictions on test fold with final model from inner loop
      if(summary == "twoClassSummary") {
        
        pred_num <- predict(inner_model, df_test, type = "prob")
        roc <- pROC::roc(df_test[[y]], pred_num[,1])$auc
        pred_class <- predict(inner_model, df_test)
        results <-  caret::confusionMatrix(pred_class
                                           ,reference = df_test[[y]]
                                           ,mode = "everything")
        df_results$outer_mean_sens[k] <- results$byClass[1]
        df_results$outer_mean_spec[k] <- results$byClass[2]
        df_results$outer_mean_auc[k] <- roc
        list_pred[[fold_name]] <- data.frame(row_num = test_rows
                                             ,pred = pred_num
                                             ,obs = df_test[[y]])
        cat("\nOuter loop results: ROC = ", round(roc, 4))
        
      } else if(summary == "multiClassSummary") {
        
        pred_class <- predict(inner_model, df_test)
        results <- caret::confusionMatrix(pred_class
                                          ,reference = df_test[[y]]
                                          ,mode = "everything")
        df_results$outer_mean_sens[k] <- mean(results$byClass[,1])
        df_results$outer_mean_spec[k] <- mean(results$byClass[,2])
        df_results$outer_mean_ppv[k] <- mean(results$byClass[,3])
        df_results$outer_mean_npv[k] <- mean(results$byClass[,4])
        df_results$outer_mean_f1[k] <- mean(results$byClass[,7])
        df_results$outer_mean_balanced_acc[k] <- mean(results$byClass[,11])
        list_pred[[fold_name]] <- data.frame(row_num = test_rows
                                             ,pred = pred_class
                                             ,obs = df_test[[y]])
        cat("\nOuter loop results: BA = ", round(mean(results$byClass[,11]), 4))
      }
      
      if(k == 1 && rep == 1) {
        time_end <- Sys.time()
        time_diff <- difftime(time_end, time_start, units = "min")
        time_total <- round(time_diff * (outer_k * outer_rep), 2)
        cat(paste0("; Estimated completion time = ", time_total, " min"))
      }
      
    }
    
    list_results[[rep]] <- df_results
    
  }
  
  df_results_final <- dplyr::bind_rows(list_results)
  if(summary == "twoClassSummary") {
    results_mean <- apply(df_results_final[,3:6], 2, mean)
  } else if(summary == "multiClassSummary") {
    results_mean <- apply(df_results_final[,3:9], 2, mean)
  }
  
  if (feat_select == TRUE) {
    return(list(resample = df_results_final
                ,average = results_mean
                ,model = method
                ,sampling = sampling
                ,outer_CV = paste0(outer_k, " X ", outer_rep)
                ,inner_CV = paste0(inner_k, " X ", inner_rep)
                ,random_seed = seed
                ,predictions = list_pred
                ,features = list_features)
    )
  } else {
    return(list(resample = df_results_final
                ,average = results_mean
                ,model = method
                ,sampling = sampling
                ,outer_CV = paste0(outer_k, " X ", outer_rep)
                ,inner_CV = paste0(inner_k, " X ", inner_rep)
                ,random_seed = seed
                ,predictions = list_pred)
    )
  }
  
}