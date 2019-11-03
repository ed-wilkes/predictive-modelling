#' @name plotConfMatrix
#' @author Ed Wilkes
#' 
#' @description Plots confusion matrix 'predictions_class' object  
#' 
#' @param conf_mat Data frame containing confusion matrix to be plotted
#' @param breaks Breaks for heatmap colours, defaults to seq(0, 100, 5)
#' @param colours Colours for heatmap, defaults to c("white", "red2")
#'
#' @return gplots heatmap object
#' 
plotConfMatrix <- function(data
                           ,breaks = seq(0, 100, 5)
                           ,colours = c("white", "red2")) {

  ## Required packages                                                        
  require(caret)
  require(dplyr)
  require(gplots)
  require(stringr)
  require(reshape2)
  
  ## Manipulate data for plotting
  data <- data %>%
    mutate(rep = substr(fold, str_locate(fold, "\\.") + 1, nchar(fold)))
  data_split <- split(data, data$rep)
  list_results <- lapply(data_split, function(x) {
    conf_mat <- as.data.frame(confusionMatrix(data = x$pred
                                              ,reference = x$obs)$table)
    return(conf_mat)
  })
  
  ## Average confusion matrices for each repeat
  df_all <- bind_rows(list_results) %>%
    group_by(Reference, Prediction) %>%
    summarise(Freq = round(mean(Freq), 0))
  conf_mat <- reshape2::dcast(df_all, Reference ~ Prediction, value.var = "Freq")
  
  ## Plot resulting data
  rownames(conf_mat) <- conf_mat[,1]
  conf_mat <- conf_mat[,-1]
  conf_mat <- as.matrix(conf_mat)
  conf_mat_ratio <- sweep(conf_mat, 1, rowSums(conf_mat), `/`)*100
  hm_breaks <- breaks
  hm_colours <- colorRampPalette(colours)(length(hm_breaks)-1) 
  hm_pred <- heatmap.2(conf_mat_ratio
                       ,trace = "none"
                       ,dendrogram = "none"
                       ,Colv = FALSE
                       ,Rowv = FALSE
                       ,col = hm_colours
                       ,breaks = hm_breaks
                       ,cellnote = conf_mat
                       ,notecol = "black"
  )
  
}