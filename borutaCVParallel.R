#' @name borutaCVParallel
#' @author Ed Wilkes
#' 
#' @description performs a k-fold cross-validated Boruta analysis 
#' 
#' @param data Data frame containing data to model
#' @param y String denoting the name of the outcome variable in the input data
#' @param outer_k Numeric denoting the number of folds in the outer CV loop
#' @param outer_rep Numeric denoting the number of times to repeat the outer CV 
#' @param inner_k Numeric denoting the number of folds in the inner CV loop
#' @param inner_rep Numeric denoting the number of times to repeat the inner CV
#' @param seed Numeric giving the random seed to set
#' @param max_runs Numeric denoting the maximum number of iterations to perform for 
#' Boruta feature selection
#' @param n_cores Numeric denoting the number of cores to use for parallel processing
#' 
#' @return List with the following objects:
#'           (i) List of selected features from each fold/repeat
#'           (ii) List of saved feature importances from each Boruta run
#'
borutaCVParallel <- function(data
                             ,y
                             ,outer_k
                             ,outer_rep
                             ,seed = 123
                             ,max_runs = 100
                             ,n_cores = 2) {
  
  ## Required packages
  require(Boruta)
  require(doParallel)
  require(doRNG)
  require(dplyr)
  require(caret)
  require(foreach)
  require(reshape2)
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
  
  ## Check that folds/reps have been defined, default if not ----
  if (missing(outer_k)) {
    warning("No value given for 'outer_k', defaulting to 2.")
    outer_k <- 2
  }
  if (missing(outer_rep)) {
    warning("No value given for 'outer_rep', defaulting to 5.")
    outer_rep <- 5
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
  
  ## Outer CV repeat loop ----
  message("Performing CV Boruta analysis ...")
  cross_validation <- 
    foreach(i = iter(names(list_folds))
            ,.combine = rbind
            ,.options.RNG = seed
            ,.packages = "dplyr") %dorng% {
              
              # Create train and test data
              rows_train <- list_folds[[i]]
              df_train <- data[rows_train,]
              df_test <- data[-rows_train,]
              test_rows <- as.numeric(row.names(df_test))
              
              # Select features with Boruta
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
              df_importance <- model$ImpHistory %>%
                as.data.frame() %>%
                reshape2::melt()

              return(list(selected_features = list_features
                          ,feature_importance = df_importance))
              
            }
  
  return(list(features = cross_validation[,1]
              ,importance = cross_validation[,2])
  )
  
  stopCluster(cluster)
  
}
