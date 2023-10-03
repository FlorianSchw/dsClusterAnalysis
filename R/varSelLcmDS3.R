#'
#' @import VarSelLCM
#' @import dplyr
#' @import truncnorm
#' @import clusterSim
#' @export
#' 


varSelLcmDS3 <- function(df){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  cluster_pre <- eval(parse(text="cluster_pre"), envir = parent.frame())[[1]]
  FinalResults <- eval(parse(text="cluster_pre"), envir = parent.frame())[[2]]
  
  results_values_final <- cluster_pre
  results_values <- results_values_final[1:nrow(df)]
  
  
  
  categories <- sapply(df, is.factor)
  colNum <- ncol(df)
  
  modelResults <- data.frame(df, results_values)
  
  
  initialResults_Mean <- modelResults %>%
    group_by(results_values) %>%
    mutate(across(-any_of(colnames(df[ ,categories])), as.numeric)) %>%
    summarise(across(-any_of(colnames(df[ ,categories])), ~mean(.x, na.rm = TRUE))) %>%
    rename_with(~ paste0("Mean_X_", .), -results_values)
  
  
  observations_clusters <- modelResults %>%
    group_by(results_values) %>%
    summarise(Observations = n())
  
  initialResults <- data.frame(initialResults_Mean, 
                               observations_clusters)
  
  
  # stop("after init")
  
  
  cols <- colnames(df[, categories])
  cols_levels <- list()
  
  for (i in seq_along(cols)){
    
    cols_levels[[i]] <- levels(df[[cols[i]]])
    
  }
  
  cat_names <- c()
  count <- 1
  
  for (p in seq_along(cols_levels)){
    for (k in 1:length(cols_levels[[p]])){
      
      cat_names[count] <- paste0("CAT_X_", cols[p], "_X_", cols_levels[[p]][k])
      count <- count + 1
      
    }
  }
  
  
  #### using smart round to account for NAs in dataset for factor lengths below
  smart.round <- function(x) {
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y
  }
  
  
  
  for (i in 1:length(initialResults$results_values)){
    for (p in 1:length(cat_names)){
      
      initialResults[[cat_names[p]]][i] <- length(modelResults[ , strsplit(cat_names[p], "_X_")[[1]][2]][which(modelResults$results_values == initialResults$results_values[i] & modelResults[ , strsplit(cat_names[p], "_X_")[[1]][2]] == as.numeric(strsplit(cat_names[p], "_X_")[[1]][3]))])
      
    }
  }
  
  
  probabilities <- initialResults[, cat_names]
  
  a <- strsplit(cat_names, "_X_")
  
  storing_length <- c()
  missings <- c()
  
  
  for (cc in 1:dim(probabilities)[1]){
    
    for (yy in 1:length(cols)){
      
      sum_factor <- 0
      for (i in 1:length(a)){
        
        if(a[[i]][2] == cols[yy]){
          sum_factor <- initialResults[[cat_names[i]]][cc] + sum_factor
        }
        
      }
      
      storing_length[yy] <- sum_factor
      missings[yy] <- initialResults$Observations[cc] - sum_factor
      
      indices_vector <- c()
      length_indices_vector <- 0
      
      for (k in 1:length(a)){
        
        if(a[[k]][2] == cols[yy]){
          probabilities[[cat_names[k]]][cc] <- (probabilities[[cat_names[k]]][cc] / storing_length[yy]) * missings[yy]
          indices_vector[length_indices_vector+1] <- k
          length_indices_vector <- length(indices_vector)
        }
      }
      probabilities[indices_vector][cc,] <- t(smart.round(t(probabilities[indices_vector][cc,])))
    }
  }
  
  
  
  initialResults[cat_names] <- initialResults[cat_names] + probabilities[cat_names]
  
  #### test for number of entries at this point for nfilter tab
  
  
  initialResults <- initialResults %>%
    mutate(across(all_of(cat_names), ~.x / Observations)) %>%
    select(-all_of(c("results_values.1", "Observations")))
  
  
  #stop("before dplyr")
  
  
  data_db <- df %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.character), as.numeric)) 
  
  #stop("before DB")
  
  value_DB <- clusterSim::index.DB(x = data_db,
                                   cl = cluster_pre)[[1]]
  
  outcome <- list(initialResults,
                  FinalResults@model@names.irrelevant,
                  FinalResults@criteria@discrim,
                  FinalResults@criteria@loglikelihood,
                  FinalResults@criteria@AIC,
                  FinalResults@criteria@BIC,
                  FinalResults@criteria@ICL,
                  value_DB)
  
  
  
  
  
  
  
  
  
  
  
  return(outcome)
  
  
  
  
  
}







