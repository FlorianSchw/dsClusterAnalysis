#'
#' @import VarSelLCM
#' @import dplyr
#' @export
#' 


varSelLcmDS1 <- function(df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  
  # Cuts the tree
  results <- VarSelLCM::VarSelCluster(x = df,
                                      gvals = num.clust,
                                      vbleSelec = vbleSelec,
                                      crit.varsel = crit.varsel,
                                      initModel = initModel,
                                      nbcores = nbcores,
                                      nbSmall = nbSmall,
                                      iterSmall = iterSmall,
                                      nbKeep = nbKeep,
                                      iterKeep = iterKeep,
                                      tolKeep = tolKeep) 
  
  categories <- sapply(df, is.factor)
  colNum <- ncol(df)
  results_values <- fitted(results)
  modelResults <- data.frame(df, results_values)
  
  initialResults_Mean <- modelResults %>%
    group_by(results_values) %>%
    mutate(across(-any_of(colnames(df[ ,categories])), as.numeric)) %>%
    summarise(across(-any_of(colnames(df[ ,categories])), ~mean(.x, na.rm = TRUE))) %>%
    rename_with(~ paste0("Mean_X_", .), -results_values)
  
  
  initialResults_SD <- modelResults %>%
    group_by(results_values) %>%
    mutate(across(-any_of(colnames(df[ ,categories])), as.numeric)) %>%
    summarise(across(-any_of(colnames(df[ ,categories])), ~sd(.x, na.rm = TRUE))) %>%
    rename_with(~ paste0("SD_X_", .), -results_values)
  
  
  observations_clusters <- modelResults %>%
    group_by(results_values) %>%
    summarise(Observations = n())
  
  disclosure_risk_numeric <- observations_clusters
  
  
  initialResults <- data.frame(initialResults_Mean, initialResults_SD, observations_clusters)
  
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
  
  disclosure_risk_factors <- initialResults[, cat_names]
  
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
  
  outcome <- initialResults
  
  
  #### Disclosure Risk Testing for clusters in general & factor levels
  
  cell_count_threshold <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(cell_count_threshold$nfilter.tab)
  
  invalid_clusters <- (sum(disclosure_risk_numeric$Observations < nfilter.tab &
                           disclosure_risk_numeric$Observations > 0)>=1)
  
  
  if(invalid_clusters){
    stop(paste0("Initial cluster creation caused one cluster to have between 1 and ", nfilter.tab-1, " observations."))
  }
  
  
  invalid_clusters_factors <- (sum(disclosure_risk_factors < nfilter.tab &
                                   disclosure_risk_factors > 0)>=1)
  
  
  if(invalid_clusters_factors){
    stop(paste0("Initial cluster creation caused one categorical variable to have between 1 and ", nfilter.tab-1, " observations in one cluster."))
  }
  
  
  
  
  
  # Assigns the resulting vector with the cluster numbers to the server side  
  #### should be initial Results but for testing purposes is modelResults
  return(outcome)
  
  
  
}
