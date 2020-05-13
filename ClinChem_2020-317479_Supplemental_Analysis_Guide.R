###############################################################################################
## Supplementary material to:
## A machine learning approach for the auomated interpretation of plasma amino acid profiles
## Wilkes EH, Emmett E, Beltran L, Woodward GM, Carling RS
## Clinical Chemistry (2020)
##
## Description:
## This script aims to guide readers through the analyses performed within the above paper.
## For clarity, the script does not include the WSRF and XGB analyses; however, comments
## are provided for the function arguments that need to be changed in order to reproduce these.
## Note that the WSRF and XGB algorithms take longer to train than RF.
###############################################################################################

## Load required packages ----
library(Boruta)
library(doParallel)
library(doRNG)
library(dplyr)
library(foreach)
library(gplots)
library(ggplot2)
library(MLmetrics)
library(reshape2)
library(tsne)

## Load raw data ----
df_model_binary <- read.csv(file.choose(), header = TRUE) # binary classification data
df_model_all <- read.csv(file.choose(), header = TRUE) # multi-class data

## t-SNE analysis ----

  # We must first create a copy of the data and encode the categorical variables (SEX, ASA, Allo, and Hcys) as integers
  df_model_encode <- df_model_all %>%
    select(-SID) %>%
    mutate(
      SEX = ifelse(SEX == "F", 0, ifelse(SEX == "M", 1, ifelse(SEX == "U", 2, NA)))
      ,ASA = ifelse(ASA == "N", 0, ifelse(ASA == "Y", 1, NA))
      ,Allo = ifelse(Allo == "N", 0, ifelse(Allo == "Y", 1, NA))
      ,Hcys = ifelse(Hcys == "N", 0, ifelse(Hcys == "Y", 1, NA))
    )

  # Loops across several values of perplexity and performs t-SNE analysis 
  set.seed(123) # It is important to set the random seed prior to analysis for reproducibility
  iterations <- c(5, 10, 25, 50) # the values of perplexity we will use
  num_cores <- 4 # reduce this value if your PC has < 4 virtual cores
  cluster <- makeCluster(num_cores) # create a multi-core cluster to speed up the analysis
  registerDoParallel(cluster)
  
  df_tsne <- foreach(i = iter(iterations)
                     ,.combine = rbind
                     ,.options.RNG = 123
                     ,.packages = "dplyr") %dorng% {
                       
                       tsne_results <- tsne::tsne(df_model_encode %>% 
                                                    select(-ID
                                                           ,-Class
                                                           ,-Class.Category)
                                                  ,perplexity = i
                                                  ,max_iter = 5000
                                                  ,k = 3) # maximum of three dimensions
                       
                       df_results <- data.frame(tsne_results
                                                ,Class = df_model_encode$Class
                                                ,Class.Category = df_model_encode$Class.Category
                                                ,Perp = i)
                       return(df_results)
                     }
  
  stopCluster(cluster)

  # Create a plot of the results
  colours <- rainbow(20)
  colours[20] <- "grey80" # make the "normal" class grey for clarity
  
  plot_1 <- ggplot(df_tsne, aes(x = X1, y = X2, colour = Class))+
    geom_point(alpha = 1)+
    facet_wrap(~Perp)+
    xlab("Dimension 1")+
    ylab("Dimension 2")+
    scale_colour_manual(values = colours)+
    theme_bw()+
    theme(legend.position = "none") # remove the legend for clarity
  
  # Visualise t-SNE (perplexity = 25) in three dimensions
  colours_3d <- data.frame(Class = levels(df_tsne$Class), color = colours)
  rgl::plot3d(x = filter(df_tsne, Perp == "Perplexity = 25")$X1
              ,y = filter(df_tsne, Perp == "Perplexity = 25")$X2
              ,z = filter(df_tsne, Perp == "Perplexity = 25")$X3
              ,col = colours_3d$color[match(df_tsne$Class, colours_3d$Class)]
              ,size = 8)
  rgl::play3d(rgl::spin3d())
  
## Boruta analysis of the data set ----
  
  # To gain an insight into the features that are most important for the class separation
  # we must first define the borutaCVParallel function by running the "borutaCVParallel.R"
  # script from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/borutaCVParallel.R
  
  num_cores <- 10 # change this parameter depending on how many virtual cores your PC has
                  # NB: It is best to leave some cores free (e.g., 12 - 2 = 10)
  set.seed(123) # again, this is critical to reproducibility. The value doesn't matter, it just
                # needs to be the same each time you perform an analysis
  boruta_output <- borutaCVParallel(data = df_model_all[,-c(1, 30)] # remove ID and Class columns
                                    ,y = "Class"
                                    ,outer_k = 3
                                    ,outer_rep = 5
                                    ,seed = 123
                                    ,max_runs = 200
                                    ,n_cores = num_cores)
  
  boruta_output$features # this provides the features selected in each fold/repeat
  boruta_output$importance # this provides the feature importance values calculated for each fold/repeat
    
## Perform nested CV on the binary data with random forests +/- down-sampling +/- feature selection ----
  
  # To perform the nested CV, we must first we must define the performNestedCVParallel
  # function by running the "performNestedCVParallel.R" script from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/performNestedCVParallel.R
  # The "method" argument can be changed to "wsrf" or "xgbTree" in order to fit
  # WSRF and XGB models to the data
  
  # Defining the nested CV parameters
  k_outer <- 3 # number of folds in the outer loop
  rep_outer <- 5 # number of repeats in the outer loop
  k_inner <- 2 # number of folds in the inner loop
  rep_inner <- 10 # number of repeats in the outer loop
  
  # Running the first nested CV analysis
  rf_model_binary <- performNestedCVParallel(data = df_model_binary[,-1] # remove the ID column
                                             ,y = "Class" 
                                             ,method = "rf" # use the randomForest algorithm
                                             ,metric = "AUC" # assess performance with PRAUC
                                             ,outer_k = k_outer
                                             ,outer_rep = rep_outer
                                             ,inner_k = k_inner
                                             ,inner_rep = rep_inner
                                             ,sampling = "none"
                                             ,seed = 123
                                             ,verbose = FALSE
                                             ,n_cores = num_cores 
                                             ,target_class = "X.Abnormal")
  
  # View the results
  rf_model_binary$resample # this provides the raw inner and outer loop results
  rf_model_binary$prediction_class # this provides the hard classifications for each outer loop
  rf_model_binary$prediction_prob # this provides the class probabilities for each outer loop
  rf_model_binary$hyperparameters # this provides the selected hyperparameters from the inner loop
  
  # Perform further nested CV analyses
  rf_model_binary_down <- performNestedCVParallel(data = df_model_binary[,-1]
                                                  ,y = "Class"
                                                  ,method = "rf"
                                                  ,metric = "AUC"
                                                  ,outer_k = k_outer
                                                  ,outer_rep = rep_outer
                                                  ,inner_k = k_inner
                                                  ,inner_rep = rep_inner
                                                  ,sampling = "down" # use down-sampling
                                                  ,seed = 123
                                                  ,verbose = FALSE
                                                  ,n_cores = num_cores
                                                  ,target_class = "X.Abnormal")
  
  rf_model_binary_select <- performNestedCVParallel(data = df_model_binary[,-1]
                                                    ,y = "Class"
                                                    ,method = "rf"
                                                    ,metric = "AUC"
                                                    ,outer_k = k_outer
                                                    ,outer_rep = rep_outer
                                                    ,inner_k = k_inner
                                                    ,inner_rep = rep_inner
                                                    ,sampling = "none"
                                                    ,seed = 123
                                                    ,verbose = FALSE
                                                    ,feat_select = TRUE # include feature selection
                                                    ,max_runs = 200 # perform a max of 200 Boruta runs
                                                    ,n_cores = num_cores
                                                    ,target_class = "X.Abnormal")
  
  rf_model_binary_select$features # This provides the selected features for each outer loop
                                  # This will be identical to boruta_output$features (assuming
                                  # the same seed as been used each time)
  
  rf_model_binary_down_select <- performNestedCVParallel(data = df_model_binary[,-1]
                                                         ,y = "Class"
                                                         ,method = "rf"
                                                         ,metric = "AUC"
                                                         ,outer_k = k_outer
                                                         ,outer_rep = rep_outer
                                                         ,inner_k = k_inner
                                                         ,inner_rep = rep_inner
                                                         ,sampling = "down" # use down-sampling
                                                         ,seed = 123
                                                         ,verbose = FALSE
                                                         ,feat_select = TRUE # include feature selection
                                                         ,max_runs = 200 # perform a max of 200 Boruta runs
                                                         ,n_cores = num_cores
                                                         ,target_class = "X.Abnormal")

## Perform nested CV on the multi-class data with random forests +/- down-sampling +/- feature selection ----
  
  # To use the F4-score as an optimisation metric, we must first define the new
  # caret F4 function by running the "multiClassSummaryF4.R" script from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/multiClassSummaryF4.R
  # We also need to load in the custom caret sampling method ("customSample") from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/customSample.R
  
  # Running the nested CV analyses
  rf_model_multi <- performNestedCVParallel(data = df_model_all[,-1]
                                            ,y = "Class"
                                            ,method = "rf"
                                            ,metric = "Mean_F4" # assess performance with F4 score
                                            ,outer_k = k_outer
                                            ,outer_rep = rep_outer
                                            ,inner_k = k_inner
                                            ,inner_rep = rep_inner
                                            ,sampling = "none"
                                            ,seed = 123
                                            ,verbose = FALSE
                                            ,n_cores = num_cores)
  
  rf_model_multi_down <- performNestedCVParallel(data = df_model_all[,-1]
                                                 ,y = "Class"
                                                 ,method = "rf"
                                                 ,metric = "Mean_F4"
                                                 ,outer_k = k_outer
                                                 ,outer_rep = rep_outer
                                                 ,inner_k = k_inner
                                                 ,inner_rep = rep_inner
                                                 ,sampling = "down" # down-sampling
                                                 ,seed = 123
                                                 ,verbose = FALSE
                                                 ,n_cores = num_cores)
  
  rf_model_multi_custom <-  performNestedCVParallel(data = df_model_all[,-1]
                                                    ,y = "Class"
                                                    ,method = "rf"
                                                    ,metric = "Mean_F4"
                                                    ,outer_k = k_outer
                                                    ,outer_rep = rep_outer
                                                    ,inner_k = k_inner
                                                    ,inner_rep = rep_inner
                                                    ,sampling = "custom" # custom down-sampling
                                                    ,seed = 123
                                                    ,verbose = FALSE
                                                    ,n_cores = num_cores)

  rf_model_multi_select <- performNestedCVParallel(data = df_model_all[,-1]
                                                   ,y = "Class"
                                                   ,method = "rf"
                                                   ,metric = "Mean_F4"
                                                   ,outer_k = k_outer
                                                   ,outer_rep = rep_outer
                                                   ,inner_k = k_inner
                                                   ,inner_rep = rep_inner
                                                   ,sampling = "none"
                                                   ,seed = 123
                                                   ,verbose = FALSE
                                                   ,feat_select = TRUE # include feature selection
                                                   ,max_runs = 200 # perform a max of 200 Boruta runs
                                                   ,n_cores = num_cores)
  
  rf_model_multi_down_select <- performNestedCVParallel(data = df_model_all[,-1]
                                                        ,y = "Class"
                                                        ,method = "rf"
                                                        ,metric = "Mean_F4"
                                                        ,outer_k = k_outer
                                                        ,outer_rep = rep_outer
                                                        ,inner_k = k_inner
                                                        ,inner_rep = rep_inner
                                                        ,sampling = "down" # down-sampling
                                                        ,seed = 123
                                                        ,verbose = FALSE
                                                        ,feat_select = TRUE # include feature selection
                                                        ,max_runs = 200 # perform a max of 200 Boruta runs
                                                        ,n_cores = num_cores)

  rf_model_multi_custom_select <-  performNestedCVParallel(data = df_model_all[,-1]
                                                           ,y = "Class"
                                                           ,method = "rf"
                                                           ,metric = "Mean_F4"
                                                           ,outer_k = k_outer
                                                           ,outer_rep = rep_outer
                                                           ,inner_k = k_inner
                                                           ,inner_rep = rep_inner
                                                           ,sampling = "custom" # custom down-sampling
                                                           ,seed = 123
                                                           ,verbose = FALSE
                                                           ,feat_select = TRUE # include feature selection
                                                           ,max_runs = 200 # perform a max of 200 Boruta runs
                                                           ,n_cores = num_cores)
  
## Plot the modelling results ----
  
  # You can access the results of the nested CV analysis through the saved output object;
  # however, we can also use the convenient "plotConfMatrix.R" and "plotAUC.R" functions 
  # available from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/plotConfMatrix.R
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/plotAUC.R
  # NB: plotConfMatrix computes the sum of the predictions across the folds within each 
  # repeat of the nested CV process and then averages these across the repeats
  
  # To determine the best probability threshold for hard classification from the binary
  # classifiers, we need to use the "findProbCutoff.R" and "predictBinaryClasses.R"
  # functions available from:
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/findProbCutoff.R
  # https://github.com/ed-wilkes/predictive-modelling/blob/master/predictBinaryClasses.R
  
  # Plot binary analysis data
  plot_2a <- plotAUC(data = rf_model_binary$prediction_prob
                     ,y_measure = "ppv" # positive predictive value (precision)
                     ,x_measure = "sens" # sensitivity (recall)
                     ,order = c("No.significant.abnormality.detected.", "X.Abnormal"))
  
  # Find the best class probability thresholds for each fold/repeat of the nested CV process
  rf_model_binary_threshold <- findProbCutoff(rf_model_binary$prediction_prob
                                              ,order = c("No.significant.abnormality.detected."
                                                         ,"X.Abnormal")
                                              ,measure_1 = "ppv"
                                              ,measure_2 = "sens"
                                              ,optimise = "F4") # optimise the threshold based on F4-score
  
  # Make hard classification calls for each fold/repeat based on the tuned threshold
  rf_model_binary_tuned <- predictBinaryClasses(predictions = rf_model_binary$prediction_prob
                                                ,thresholds = rf_model_binary_threshold
                                                ,target = "X.Abnormal")
    
  plot_2b <- plotConfMatrix(data = rf_model_binary_tuned)
  abline(h = 644 / (644 + 1440)) # Positive class / (Positive class + Negative class)
  
  # Plot multi-class analysis data
  plot_3 <- plotConfMatrix(data = rf_model_multi$prediction_class)
  
## Average prediction probabilities from multiple individual classifiers ----
  
  # Train models with the WSRF algorithm 
  wsrf_model_binary <- performNestedCVParallel(data = df_model_binary[,-1] 
                                               ,y = "Class" 
                                               ,method = "wsrf" # use the WSRF algorithm
                                               ,metric = "AUC" # assess performance with PRAUC
                                               ,outer_k = k_outer
                                               ,outer_rep = rep_outer
                                               ,inner_k = k_inner
                                               ,inner_rep = rep_inner
                                               ,sampling = "none"
                                               ,seed = 123
                                               ,verbose = FALSE
                                               ,n_cores = num_cores 
                                               ,target_class = "X.Abnormal")
  
  # Train models with the XGB algorithm 
  xgb_model_binary <- performNestedCVParallel(data = df_model_binary[,-1] 
                                              ,y = "Class" 
                                              ,method = "xgbTree" # use the XGB algorithm
                                              ,metric = "AUC" # assess performance with PRAUC
                                              ,outer_k = k_outer
                                              ,outer_rep = rep_outer
                                              ,inner_k = k_inner
                                              ,inner_rep = rep_inner
                                              ,sampling = "none"
                                              ,seed = 123
                                              ,verbose = FALSE
                                              ,n_cores = num_cores 
                                              ,target_class = "X.Abnormal")
  
  # Collect prediction probabilities and melt them to long format
  df_pred_model_1 <- data.frame(melt(rf_model_binary$prediction_prob
                                     ,id.vars = c("fold", "row_num", "obs"))
                                ,model = "rf")
  df_pred_model_2 <- data.frame(melt(rf_model_binary$prediction_prob
                                     ,id.vars = c("fold", "row_num", "obs"))
                                ,model = "wsrf")
  df_pred_model_3 <- data.frame(melt(rf_model_binary$prediction_prob
                                     ,id.vars = c("fold", "row_num", "obs"))
                                ,model = "xgb")
  
  # Combine the probabilities and calculate the average for each case in each fold/repeat
  df_pred_all <- bind_rows(df_pred_model_1, df_pred_model_2, df_pred_model_3)
  df_pred_averaged <- df_pred_all %>% 
    group_by(fold, obs, row_num, variable) %>% 
    summarise(mean_pred = mean(value)) %>% # calculate mean across rf, wsrf, and xgb probabilities
    ungroup()
  df_pred_all$fold <- factor(df_pred_all$fold, levels = unique(df_pred_all$fold))
  
  # Split the data into a list based on fold/repeat number
  list_pred <- split(df_pred_averaged, df_pred_averaged$fold)
  
  # Calculate the PRAUC for each fold/repeat with the new, averaged probabilities
  ensemble_prauc <- lapply(list_pred, function(x) {
    auc <- MLmetrics::PRAUC(y_pred = x$mean_pred, y_true = x$obs)
  }) %>% unlist
  
  # Calculate the mean PRAUC
  mean(ensemble_prauc)
  
