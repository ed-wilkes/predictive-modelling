## Custom sampling method for caret models

custom <- list(name = "customSample"
               ,func = function(x, y) {
                 
                 ## Check input data
                 xc <- class(x)
                 if (!is.data.frame(x)) 
                   x <- as.data.frame(x)
                 if (!is.factor(y)) {
                   warning("Custom-sampling requires a factor variable as the response. 
            The original data was returned.")
                   return(list(x = x, y = y))
                 }
                 
                 ## Sample normals
                 x$.outcome <- y
                 
                 x_minority <- dplyr::filter(x, .outcome != "No.significant.abnormality.detected.") 
                 max_freq <- max(table(x_minority$.outcome))
                 
                 x_majority <- dplyr::filter(x, .outcome == "No.significant.abnormality.detected.") %>%
                   dplyr::sample_n(max_freq, replace = FALSE) %>%
                   as.data.frame
                 
                 x <- dplyr::bind_rows(x_minority, x_majority)
                 
                 y <- x$.outcome
                 x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = TRUE]
                 
                 ## Return as list
                 out <- list(x = x, y = y)
                 
               }
               ,first = TRUE)