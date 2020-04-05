#' @name performedNestedCVParallel
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
#' @param target_class String denoting the target class for binary classifications
#' @param n_cores Numeric denoting the number of cores to use for parallel processing
#' 
#' @return List with the following objects:
#'           (i) Inner and outer CV loop resample results
#'           (ii) Averaged results
#'           (iii) Algorithm used (i.e., "method" argument)
#'           (iv) Sampling method used
#'           (v) Outer CV settings
#'           (vi) Inner CV settings
#'           (vii) Random seed setting
#'           (viii) List of class predictions from each outer fold/repeat
#'           (ix) List of class probabilites from each outer fold/repeat
#'           (x) Data frame hyperparameters selected for each outer fold/repeat
#'           (xi) List of selected features (if feat_select == TRUE)
#'
#' To do:
#'   - Implement used of features selected from separate cross-validated Boruta analysis
#'     to save run time
#'   
performNestedCVParallel <- function(data
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
                                    ,max_runs = 100
                                    ,n_cores = 2
                                    ,target_class = NULL) {
  
  ## Required packages
  require(Boruta)
  require(doParallel)
  require(doRNG)
  require(dplyr)
  require(caret)
  require(foreach)
  require(MLmetrics)
  require(pROC)
  require(stringr)
  
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
  if(length(levels(data[[y]])) > 2 && !grepl("Mean_F", metric)) {
    
    message("Outcome vector has more than one level, assuming multiclass analysis ...")
    summary <- "multiClassSummary"
    
  } else if (length(levels(data[[y]])) > 2 && grepl("Mean_F", metric)) {
    
    message("Outcome vector has more than one level, assuming multiclass analysis ...")
    beta <- stringr::str_sub(metric, nchar(metric))
    if (as.numeric(beta) > 1) {
      
      func_name <- paste0("multiClassSummaryF", beta)
      if (!beta %in% c("2", "4")) {
        stop(paste0("Only F1, F2, or F4 scores are currently supported."))
      } else if (is.null(get(func_name))) {
        stop(paste0("This metric requires the summaryFunction '"
                    ,func_name
                    ,"in your global environment."))
      } else {
        summary <- func_name
      }
      
    } else {
      summary <- "multiClassSummary"
    }
    
  } else if (length(levels(data[[y]])) == 2 && metric != "AUC") {
    
    message("Outcome vector has two levels, assuming two class analysis ...")
    summary <- "twoClassSummary"
    
  } else if (length(levels(data[[y]])) == 2 && metric == "AUC") {
    
    message("Outcome vector has two levels, assuming two class analysis ...")
    summary <- "prSummary"
    
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
  if (feat_select == TRUE) {
    message("Feature selection is enabled, this will substantially increase run time!")
  }
  
  ## Check if sampling is correct string ----
  if (!sampling %in% c("none", "down", "smote", "rose", "custom")) {
    stop("'sampling' must be a string of value 'none', 'down', 'smote', 'rose', or 'custom'.")
  }
  
  ## Check if target_class has been defined ----
  n_class <- length(unique(data[[y]]))
  if (is.null(target_class) && n_class == 2) {
    stop("For two class comparisons, you must define a target (i.e., positive) class!")    
  }

  ## Define inner CV control ----
  if (sampling == "none") {
    
    inner_control <- caret::trainControl(method = "repeatedcv"
                                         ,number = inner_k
                                         ,repeats = inner_rep
                                         ,verboseIter = verbose
                                         ,classProbs = TRUE
                                         ,allowParallel = FALSE
                                         ,trim = TRUE
                                         ,summaryFunction = get(summary))
    
  } else if (sampling %in% c("down", "smote", "rose")) {
    
    inner_control <- caret::trainControl(method = "repeatedcv"
                                         ,number = inner_k
                                         ,repeats = inner_rep
                                         ,verboseIter = verbose
                                         ,classProbs = TRUE
                                         ,sampling = sampling
                                         ,allowParallel = FALSE
                                         ,trim = TRUE
                                         ,summaryFunction = get(summary))
    
  } else if (sampling == "custom") {
    
    if (is.null(get(sampling))) {
      stop("This sampling scheme requires a caret sampling method called 'custom' in your global environment.")
    } else {
      inner_control <- caret::trainControl(method = "repeatedcv"
                                           ,number = inner_k
                                           ,repeats = inner_rep
                                           ,verboseIter = verbose
                                           ,classProbs = TRUE
                                           ,sampling = get(sampling)
                                           ,allowParallel = FALSE
                                           ,trim = TRUE
                                           ,summaryFunction = get(summary))
    }
    
  }
  
  ## Set seed and define outer folds for each repeat ----
  set.seed(seed)
  list_folds <- caret::createMultiFolds(y = data[[y]], k = outer_k, times = outer_rep)
  
  ## Set up cluster ----
  cores_max <- detectCores()
  if (n_cores > cores_max) {
    stop("Number of cores detected is lower than n_cores!")
  } else {
    message(paste0("Running on ", n_cores, " cores ..."))
  }
  cluster <- makeCluster(n_cores)
  registerDoParallel(cluster)
  
  ## Export necessary functions to cluster ----
  if (sampling == "custom") {
    clusterExport(cluster, varlist = "custom", envir = .GlobalEnv)
  }
  if (grepl("multiClassSummaryF", summary)) {
    clusterExport(cluster, varlist = summary, envir = .GlobalEnv)
  }
  
  ## Outer CV repeat loop ----
  message("Performing nested CV ...")
  cross_validation <- 
    foreach(i = iter(names(list_folds))
            ,.combine = rbind
            ,.options.RNG = seed
            #,.export = sampling
            ,.packages = "dplyr") %dorng% {
              
              # Initialise results data frame
              df_results <- data.frame(fold = i)
              
              # Create train and test data
              rows_train <- list_folds[[i]]
              df_train <- data[rows_train,]
              df_test <- data[-rows_train,]
              test_rows <- as.numeric(row.names(df_test))
              
              # Select features if required
              if(feat_select == TRUE) {
                
                model <- Boruta::Boruta(as.formula(paste(paste(y),"~ ."))
                                        ,df_train
                                        ,doTrace = 0
                                        ,maxRuns = max_runs) %>%
                  Boruta::TentativeRoughFix()
                
                vars_boruta <- c(Boruta::getSelectedAttributes(model
                                                               ,withTentative = FALSE)
                                 ,y)
                
                df_train <- df_train[, which(colnames(df_train) %in% vars_boruta)]
                list_features <- list(vars_boruta)
                names(list_features) <- i
                
              }
              
              # Inner CV loop (for hyperparameter tuning) defined by trControl argument
              inner_model <- caret::train(form = as.formula(paste(paste(y),"~ ."))
                                          ,data = df_train
                                          ,method = method
                                          ,metric = metric
                                          ,trControl = inner_control)
              best_hyper <- inner_model$bestTune
              
              # Get predictions on test fold with final model from inner loop
              if (summary %in% c("twoClassSummary", "prSummary")) {
                
                pred_prob <- predict(inner_model, df_test, type = "prob")
                roc <- pROC::roc(df_test[[y]], pred_prob[[target_class]])$auc
                pred_class <- predict(inner_model, df_test)
                results <-  caret::confusionMatrix(pred_class
                                                   ,reference = df_test[[y]]
                                                   ,mode = "everything")
                best_hyper <- inner_model$bestTune
                
                df_results[[paste0("inner_", metric)]] <- 
                  round(caret::getTrainPerf(inner_model)[[paste0("Train", metric)]], 4) %>% 
                  as.numeric
                df_results$outer_mean_sens <- results$byClass[1]
                df_results$outer_mean_spec <- results$byClass[2]
                df_results$outer_mean_ppv <- results$byClass[3]
                df_results$outer_mean_npv <- results$byClass[4]
                df_results$outer_mean_auroc <- roc
                
                if (metric == "AUC") {
                  prauc <- MLmetrics::PRAUC(y_pred = pred_prob[[target_class]]
                                            ,y_true = df_test[[y]])
                  df_results[[paste0("outer_pr", metric)]] <- round(prauc, 4)
                }
                
                df_pred_class <- data.frame(fold = i
                                            ,row_num = test_rows
                                            ,pred = pred_class
                                            ,obs = df_test[[y]])
                
                df_pred_prob <- data.frame(fold = i
                                           ,row_num = test_rows
                                           ,pred = pred_prob[[target_class]]
                                           ,obs = df_test[[y]])
                
              } else if (summary %in% c("multiClassSummary"
                                        ,"multiClassSummaryF2"
                                        ,"multiClassSummaryF4")) {
                
                pred_class <- predict(inner_model, df_test)
                pred_prob <- predict(inner_model, df_test, type = "prob")
                results <- caret::confusionMatrix(pred_class
                                                  ,reference = df_test[[y]]
                                                  ,mode = "everything")
                
                df_results[[paste0("inner_", metric)]] <- 
                  round(caret::getTrainPerf(inner_model)[[paste0("Train", metric)]], 4) %>% 
                  as.numeric
                df_results$outer_mean_sens <- mean(results$byClass[,1])
                df_results$outer_mean_spec <- mean(results$byClass[,2])
                df_results$outer_mean_ppv <- mean(results$byClass[,3], na.rm = TRUE)
                df_results$outer_mean_npv <- mean(results$byClass[,4])
                df_results$outer_mean_f1 <- mean(results$byClass[,7], na.rm = TRUE)
                df_results$outer_mean_balanced_acc <- mean(results$byClass[,11])
                
                if (metric == "logLoss") {
                  
                  mlogloss <- ModelMetrics::mlogLoss(actual = df_test[[y]]
                                                     ,predicted = pred_prob)
                  df_results[[paste0("outer_", metric)]] <- round(mlogloss, 4)
                  
                } else if (grepl("Mean_F", metric)) {
                  
                  beta <- as.numeric(stringr::str_sub(metric, nchar(metric)))
                  precision <- results$byClass[,5]
                  recall <- results$byClass[,6]
                  f <- (1 + (beta^2)) * ((precision * recall)/((beta^2) * precision + recall))
                  df_results[[paste0("outer_", metric)]] <- round(mean(f, na.rm = TRUE), 4)
                  
                } 
                
                df_pred_class <- data.frame(fold = i
                                            ,row_num = test_rows
                                            ,obs = df_test[[y]]
                                            ,pred = pred_class)
                
                df_pred_prob <- data.frame(fold = i
                                          ,row_num = test_rows
                                          ,obs = df_test[[y]]
                                          ,pred_prob)
                
              }
              
              if (feat_select == TRUE) {
                
                return(list(cv_results = df_results
                            ,prediction_class = df_pred_class
                            ,prediction_prob = df_pred_prob
                            ,hyperparameters = best_hyper
                            ,selected_features = list_features))
                
              } else {
                
                return(list(cv_results = df_results
                            ,prediction_class = df_pred_class
                            ,prediction_prob = df_pred_prob
                            ,hyperparameters = best_hyper))  
                
              }
              
            }
  
  
  df_results_final <- dplyr::bind_rows(cross_validation[,1])
  df_pred_class_final <- dplyr::bind_rows(cross_validation[,2])
  df_pred_prob_final <- dplyr::bind_rows(cross_validation[,3])
  df_hyperparameters <- dplyr::bind_rows(cross_validation[,4])
  mean_results <- apply(df_results_final[,2:dim(df_results_final)[2]]
                        ,2
                        ,function(x) mean(x, na.rm = TRUE))
                        
  if (feat_select == TRUE) {
    return(list(resample = df_results_final
                ,average = mean_results
                ,model = method
                ,sampling = sampling
                ,outer_CV = paste0(outer_k, " X ", outer_rep)
                ,inner_CV = paste0(inner_k, " X ", inner_rep)
                ,random_seed = seed
                ,prediction_class = df_pred_class_final
                ,prediction_prob = df_pred_prob_final
                ,hyperparameters = df_hyperparameters
                ,features = cross_validation[,5]
                ,feat_select = feat_select)
    )
  } else {
    return(list(resample = df_results_final
                ,average = mean_results
                ,model = method
                ,sampling = sampling
                ,outer_CV = paste0(outer_k, " X ", outer_rep)
                ,inner_CV = paste0(inner_k, " X ", inner_rep)
                ,random_seed = seed
                ,prediction_class = df_pred_class_final
                ,prediction_prob = df_pred_prob_final
                ,hyperparameters = df_hyperparameters)
    )
  }
  
  stopCluster(cluster)
  
}