#'
#' @import VarSelLCM
#' @import dplyr
#' @import truncnorm
#' @export
#' 


varSelLcmDS2 <- function(df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  list_other_study_data <- objects(pattern = "StudyData", envir = parent.frame())
  
  
  assignments <- list()
  summaries <- list()
  
  
  check_df_total <- list()
  
  for (ww in 1:num.iterations){
    
    
    
  
  
  extension_dfs <- list()
  for (qqq in 1:length(list_other_study_data)){
    
    

  
  data_structure <- eval(parse(text=list_other_study_data[qqq]), envir = parent.frame())  
  variable_columns <- strsplit(colnames(data_structure)[which(!(grepl("results_|Observations", colnames(data_structure))))], "_X_", fixed = TRUE)
  
  type <- c()
  variables <- c()
  expression <- c()
  for (i in 1:length(variable_columns)){
    
    type[i] <- variable_columns[[i]][1]
    variables[i] <- variable_columns[[i]][2]
    expression[i] <- variable_columns[[i]][3]
    
  }
  
  variable_overview <- data.frame(type, variables, expression)
  
  
  not_categ <- variable_overview %>%
    filter(!type == "CAT")
  
  categ <- variable_overview %>%
    filter(type == "CAT")
  
  variable_cont <- unique(not_categ$variables)
  
  variable_cat <- unique(categ$variables)
  
  zzz <- list()
  data_extension <- df[1:sum(data_structure$Observations),]
  data_extension[,] <-  NA
  
  for (p in 1:length(variable_cont)){
    for (j in 1:dim(data_structure)[1]) {
      
      mean <- data_structure[[paste0("Mean_X_",variable_cont[p])]][j]
      sd <- data_structure[[paste0("SD_X_",variable_cont[p])]][j]
      length_1 <- data_structure$Observations[j]
      var_min <- min(df[[variable_cont[p]]])
      var_max <- max(df[[variable_cont[p]]])
      zzz[[j]] <- rtruncnorm(length_1, mean = mean, sd = sd, a = var_min, b = var_max)
      
    }
    
    tmp_collect <- unlist(zzz)
    data_extension[[variable_cont[p]]] <- tmp_collect
    
  }
  
  for (p in 1:length(variable_cat)){
    
    current_cat_variable <- data_structure[which(grepl(paste0("_X_" , variable_cat[p], "_X_"), colnames(data_structure), fixed = TRUE))]
    uuu <- list()

    for (j in 1:dim(data_structure)[1]){
      
      lll <- list()

      for (k in 1:dim(current_cat_variable)[2]){
      
        lll[[k]] <- rep(as.numeric(strsplit(colnames(current_cat_variable[k]), "_X_")[[1]][3]), current_cat_variable[j,k])
    
      } 
      
      tmp_cat <- sample(unlist(lll))
      uuu[[j]] <- tmp_cat
      
    }
    
    tmp_coll <- unlist(uuu)
    data_extension[[variable_cat[p]]] <- as.factor(tmp_coll)
    
  }
  
  for (u in 1:length(colnames(df))){
    
    if(class(df[[u]]) == "integer"){
      
      data_extension[[u]] <- as.integer(round(data_extension[[u]]))
     
    }
    
  }
  
  
  
  extension_dfs[[qqq]] <- data_extension
  
  
  
  #### qqq ends here
  
  }
  
  
  data_extension_full <- bind_rows(extension_dfs)
  
  dataframe_pooled <- rbind(df, data_extension_full)
  
  
  set.seed(42)
  FinalResults <- VarSelLCM::VarSelCluster(x = dataframe_pooled,
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
  
  set.seed(NULL)
  
  check_df_total[[ww]] <- dataframe_pooled
  


  ##### Here needs to be the end of the iteration process
  
  tryCatch({
    
  
  
  categories_final <- sapply(dataframe_pooled, is.factor)
  results_values_final <- fitted(FinalResults)
  modelResults_final <- data.frame(dataframe_pooled, results_values_final)
  
  
  FinalResults_Mean <- modelResults_final %>%
    group_by(results_values_final) %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise(across(all_of(variable_cont), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    rename_with(~ paste0("Mean_X_", .), -results_values_final)
  


  
  observations_clusters_final <- modelResults_final %>%
    group_by(results_values_final) %>%
    summarise(Observations = n())
  
  FinalResults_DF <- data.frame(FinalResults_Mean, observations_clusters_final)
  
  cols <- colnames(dataframe_pooled[, categories_final])
  cols_levels <- list()
  
  for (i in seq_along(cols)){
    
    cols_levels[[i]] <- levels(dataframe_pooled[[cols[i]]])
    
  }
  
  cat_names <- c()
  count <- 1
  
  for (p in seq_along(cols_levels)){
    for (k in 1:length(cols_levels[[p]])){
      
      cat_names[count] <- paste0("CAT_X_", cols[p], "_X_", cols_levels[[p]][k])
      count <- count + 1
      
    }
  }
  
  
  for (i in 1:length(FinalResults_DF$results_values_final)){
    for (p in 1:length(cat_names)){
      
      FinalResults_DF[[cat_names[p]]][i] <- length(modelResults_final[ , strsplit(cat_names[p], "_X_")[[1]][2]][which(modelResults_final$results_values_final == FinalResults_DF$results_values_final[i] & modelResults_final[ , strsplit(cat_names[p], "_X_")[[1]][2]] == as.numeric(strsplit(cat_names[p], "_X_")[[1]][3]))])/length(FinalResults_DF$Observations)
      
    }
  }
  
  #### adjustment for NA calculations
  
 # for (uu in 1:length(cols)){
    
  #  current_vector <- FinalResults_DF %>%
   #   select(contains(cols[uu])) %>%
    #  rowSums()
    
    #FinalResults_DF <- FinalResults_DF %>%
     # mutate(across(contains(cols[uu]), ~ .x / current_vector * 100))
    
  #}
  
  
  FinalResults_DF <- subset(FinalResults_DF, select = -c(results_values_final.1,
                                                         Observations))
  
  
  assignments[[ww]] <- fitted(FinalResults)
  summaries[[ww]] <- FinalResults_DF
  
  
  
  },
  #### Placeholder for better version
  error = function(e){
    message("DS2 Error in Server")
    print(e)
  }
  )
  
  
  }
  
  
  if(length(assignments) == 1){
    #assignments_dataframe <- assignments[[1]]
    summaries_dataframe <- summaries[[1]]
  }
  
  if(!(length(assignments) == 1)){
    #assignments_dataframe <- bind_rows(lapply(assignments, as.data.frame.list))
    summaries_dataframe <- bind_rows(summaries)
  }
  
  
  
  matching_vector <- rep(NA, dim(summaries_dataframe)[1])

  for (n in 1:num.clust){
    
    first_iteration <- seq(from = 1, to = num.clust)
    additional_iteration <- seq(from = (n-1)*num.clust + 1, to = n*num.clust)

    matching_indiv <- VarSelLCM::VarSelCluster(x = summaries_dataframe[c(first_iteration, additional_iteration),-c(1)],
                                               gvals = num.clust)@partitions@zMAP
    
    matching_vector[first_iteration] <- 1:num.clust
    
    iter1 <- matching_indiv[seq(from = 1, to = num.clust)]
    iter2 <- matching_indiv[seq(from = num.clust + 1, to = 2*num.clust)]
    
    pos <- c()
    for (ll in 1:length(iter2)){
      
      pos[ll] <- which(iter1 == iter2[ll])
      
    }
    matching_vector[additional_iteration] <- pos
  }
  

  summaries_dataframe$matching <- matching_vector
  
  #dist_obj <- dist(summaries_dataframe[-1])
  #hc_object <- hclust(dist_obj)
  #summaries_dataframe$matching <- cutree(hc_object, k = num.clust)
  

  collect <- list()
  
  for(pp in 1:length(assignments)){
    match <- rep(NA,length(assignments[[1]]))
    int.var <- data.frame(assignments[[pp]], match)
    
    for (ee in 1:length(assignments[[1]])){
      
      
      int.var$match[ee] <- summaries_dataframe$matching[(1+(pp-1)*num.clust):(num.clust+(pp-1)*num.clust)][which(summaries_dataframe$results_values_final[(1+(pp-1)*num.clust):(num.clust+(pp-1)*num.clust)] == int.var[ee,1])]
      
      
    }
    
    int.var <- subset(int.var, select = c(2))
    collect[[pp]] <- int.var
  }
  
  collect_dataframe <- bind_cols(collect)
  
  
  cluster_intermediate <- as.numeric(apply(collect_dataframe[1:nrow(df),], 1, function(x) names(which.max(table(x)))))
  
  
  outcome <- list(matching_vector,
                  summaries_dataframe,
                  assignments)
  
  
  return(outcome)
  
  
  
  

}
  
  
  
  

