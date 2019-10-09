#' @name performedMultiNestedCV
#' @author Ed Wilkes
#' 
#' @description performs nested CV, using the inner loop to select the optimal hyper
#' -parameters, and the outer loop to test the optimal model on a test set. This function
#' can take multiple machine learning algorithms as an input and then take the average of the
#' resulting predictions to produce a model ensemble. 
#' 
#' @param data Data frame containing data to model
#' @param y String denoting the name of the outcome variable in the input data
#' @param methods Vector of strings denoting the machine learning algorithms to use
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
#'           (iii) Algorithms used (i.e., "methods" argument)
#'           (iv) Sampling method used
#'           (v) Outer CV settings
#'           (vi) Inner CV settings
#'           (vii) Random seed setting
#'           (viii) List of predictions for each method from each outer fold/repeat
#'           (ix) List of average predictions across each method
#'
#' (c) Copyright Ed Wilkes 2018 - do not distribute this code without the author's
#' permission
#' 
performMultiNestedCV <- function(data
                                 ,y
                                 ,methods
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
  require(reshape2)
  require(tidyr)
  
  ## Check if "y" is in data ----
  if (!y %in% colnames(data)) {
    stop(paste0("Your outcome variable, '", y, "', is not in the dataset"))
  }
  
  ## Check if "y" is a factor ----
  if (!is.factor(data[[y]])) {
    message("Outcome variable is not a factor, coercing ...")
    data[[y]] <- factor(data[[y]])
  }
  
  ## Check if "verbose" is boolean ----
  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical.")
  }
  
  ## Check levels in outcome vector ----
  if (length(levels(data[[y]])) > 2) {
    message("Outcome vector has more than one level, assuming multiclass analysis ...")
    summary <- "multiClassSummary"
  } else {
    message("Outcome vector has two levels, assuming two class analysis ...")
    summary <- "twoClassSummary"
  }
  
  ## Check that folds/reps have been defined, default if not ----
  if (missing(outer_k)) {
    warning("No value given for 'outer_k', defaulting to 2.")
    outer_k <- 2
  }
  if (missing(outer_rep)) {
    warning("No value given for 'outer_rep', defaulting to 5.")
    outer_rep <- 5
  } 
  if (missing(inner_k)) {
    warning("No value for 'inner_k', defaulting to 2.")
    inner_k <- 2
  }
  if (missing(inner_rep)) {
    warning("No value for 'inner_rep', defaulting to 5.")
    inner_rep <- 5
  }
  
  ## Warn if feature selection is requested ----
  if(feat_select == TRUE) {
    message("Feature selection is enabled, this will substantially increase run time!")
  }
  
  ## Check if sampling is correct string ----
  if (!sampling %in% c("none", "down", "smote", "rose")) {
    stop("'sampling' must be a string of value 'none', 'down', 'smote', or 'rose'.")
  }
  
  ## Define inner CV control ----
  if (sampling == "none") {
    
    inner_control <- caret::trainControl(method = "repeatedcv"
                                         ,number = inner_k
                                         ,repeats = inner_rep
                                         ,verboseIter = verbose
                                         ,classProbs = TRUE
                                         ,trim = TRUE
                                         ,summaryFunction = match.fun(summary))
    
  } else if (sampling != "none") {
    
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
  
  ## Create results lists ----
  list_results <- list()
  list_pred_total <- list()
  list_pred_avg <- list()
  list_features <- list()
  
  ## Outer CV repeat loop ----
  for (rep in 1:outer_rep) {
    
    # Initialise results data frame
    if (summary == "twoClassSummary") {
      
      df_results <- data.frame(fold = 1:outer_k
                               ,rep = rep(rep, times = outer_k))
      
      for(i in methods) {
        df_results[paste0('inner_mean_auc_', i)] = NA
        df_results[paste0('outer_mean_auc_', i)] = NA
      }
      
    } else if (summary == "multiClassSummary") {
      
      df_results <- data.frame(fold = 1:outer_k
                               ,rep = rep(rep, times = outer_k))
      
      for (i in methods) {
        df_results[paste0("inner_mean_balanced_acc_", i)] <- NA
        df_results[paste0("outer_mean_balanced_acc_", i)] <- NA
        df_results[paste0("outer_mean_f1_", i)] <- NA
      }
      
    }
    
    # Outer CV loop (test set evaluation) ----
    for (k in 1:outer_k) {
      
      # Get row numbers of train data
      if (rep < 10 && outer_rep >= 10) {
        fold_name <- paste0("Fold", k, ".Rep0", rep)
      } else {
        fold_name <- paste0("Fold", k, ".Rep", rep)
      }
      rows_train <- list_folds[[fold_name]]
      
      message(paste0("\nOuter loop: ", fold_name, " processing ..."))
      if (k == 1 && rep == 1) {
        time_start <- Sys.time()
      }
      
      # Create train and test data
      df_train <- data[rows_train,]
      df_test <- data[-rows_train,]
      test_rows <- as.numeric(row.names(df_test))
      
      # Select features if required
      if (feat_select == TRUE) {
        
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
      
      # Generate lists to store predidct() results for each method
      list_pred_methods <- list() 
      for (method in methods) {
        list_pred_methods[[method]] <- data.frame(row_num = test_rows
                                                       ,obs = df_test[[y]]
                                                       ,method = method)
        # Force factors to character vectors
        list_pred_methods[[method]] %>% mutate_if(is.factor, as.character) -> 
          list_pred_methods[[method]]
      }
      
      # Loop across all defined methods and create inner models for each
      for (method in methods) {
        
        set.seed(seed)
        
        # Inner CV loop (for hyperparameter tuning) defined by trControl argument
        inner_model <- caret::train(form = as.formula(paste(paste(y),"~ ."))
                                    ,data = df_train
                                    ,method = method
                                    ,metric = metric
                                    ,trControl = inner_control)
        
        cat("\nInner loop tune results for", method, ": ")
        if (summary == "twoClassSummary") {
          
          cat(paste0("ROC = "
                     ,round(getTrainPerf(inner_model)[["TrainROC"]], 4)))
          df_results[[paste0("inner_mean_auc_", method)]][k] <- 
            round(getTrainPerf(inner_model)[["TrainROC"]], 4) %>% as.numeric
          
        } else if (summary == "multiClassSummary") {
          
          cat(paste0("BA = "
                     ,round(getTrainPerf(inner_model)[["TrainMean_Balanced_Accuracy"]], 4)))
          df_results[[paste0("inner_mean_balanced_acc_", method)]][k] <- 
            round(getTrainPerf(inner_model)[["TrainMean_Balanced_Accuracy"]], 4) %>% 
            as.numeric
          
        }
        
        # Get predictions on test fold with final model from inner loops
        if (summary == "twoClassSummary") {
          
          pred_num <- predict(inner_model, df_test, type = "prob")
          roc <- pROC::roc(df_test[[y]], pred_num[,1])$auc
          pred_class <- predict(inner_model, df_test)
          results <- caret::confusionMatrix(pred_class
                                            ,reference = df_test[[y]]
                                            ,mode = "everything")
          df_results[[paste0("outer_mean_auc_", method)]][k] <- roc
          
          # Print outer loop results to console
          cat("\nOuter loop results: ROC =", round(roc, 4))
          
          # Store predictions in list_pred()
          list_pred_methods[[method]][colnames(pred_num)] <- pred_num
          
          
        } else if (summary == "multiClassSummary") {
          
          pred_class <- predict(inner_model, df_test)
          pred_num <- predict(inner_model, df_test, type = "prob")
          results <- caret::confusionMatrix(pred_class
                                            ,reference = df_test[[y]]
                                            ,mode = "everything")
          df_results[[paste0("outer_mean_f1_", method)]][k] <- mean(results$byClass[,7])
          df_results[[paste0("outer_mean_balanced_acc_", method)]][k] <- mean(results$byClass[,11])
          
          # Print outer results to console
          cat("\nOuter loop results: BA =", round(mean(results$byClass[,11]), 4))
          
          # Store predictions in list_pred()
          list_pred_methods[[method]][colnames(pred_num)] <- pred_num
          
        }
      }
      
      # Combine individual method predictions into one data frame and average
      list_pred_total[[fold_name]] <- bind_rows(list_pred_methods)
      list_pred_avg[[fold_name]] <- list_pred_total[[fold_name]] %>%
        melt(id.vars = c("row_num", "obs", "method")) %>%
        group_by(row_num, variable) %>% 
        summarise(mean_prob = mean(value)) %>%
        spread(key = variable, value = mean_prob)
      n_obs <- length(list_pred_avg[[fold_name]]$row_num)
      list_pred_avg[[fold_name]]$obs <- list_pred_total[[fold_name]]$obs[1:n_obs]
      
      # Calculate estimated completion time
      if (k == 1 && rep == 1) {
        time_end <- Sys.time()
        time_diff <- difftime(time_end, time_start, units = "min")
        time_total <- round(time_diff * (outer_k * outer_rep), 2)
        cat(paste0("; Estimated completion time = ", time_total, " min"))
      }
      
    }
    
    list_results[[rep]] <- df_results
    
  }
  
  # Summarise average of each fold/repeat's results
  df_results_final <- dplyr::bind_rows(list_results)
  if (summary == "twoClassSummary") {
    results_mean <- apply(df_results_final[,3:dim(df_results_final)[2]], 2, mean)
  } else if (summary == "multiClassSummary") {
    results_mean <- apply(df_results_final[,3:dim(df_results_final)[2]], 2, mean)
  }
  
  # Generate vector of strings corresponding to each model's predictions
  str_model_pred <- c()
  for (i in 1:length(methods)) {
    str_model_pred[i] <- paste0("pred_", methods[i])
  }
  
  # Return results, etc. in output list
  return(list(resample = df_results_final
              ,average = results_mean
              ,models = methods
              ,sampling = sampling
              ,outer_CV = paste0(outer_k, " X ", outer_rep)
              ,inner_CV = paste0(inner_k, " X ", inner_rep)
              ,random_seed = seed
              ,predictions = list_pred_total
              ,avg_predictions = list_pred_avg
              ,feature_selection = feat_select
              ,features = list_features)
  )
  
}