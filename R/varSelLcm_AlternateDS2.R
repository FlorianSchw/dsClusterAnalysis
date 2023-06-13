#'
#' @import VarSelLCM
#' @import dplyr
#' @import truncnorm
#' @export
#' 


varSelLcm_AlternateDS2 <- function(df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  list_other_study_data <- objects(pattern = "StudyData", envir = parent.frame())
  
  
  assignments <- list()
  summaries <- list()
  store_dfs <- list()
  
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
          
          
          if(mean - 2*sd < 0){
            var_min <- 0
          } else {
            var_min <- mean - 2*sd
          }
          
          var_max <- mean + 2*sd
          
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
    
    store_dfs[[ww]] <- data_extension_full
    
  }
    
  additional_dfs <- bind_rows(store_dfs)
  dataframe_pooled <- rbind(df, additional_dfs)
    
    
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
    

   results_values_final <- fitted(FinalResults)
    
   cluster_intermediate <- results_values_final[1:nrow(df)]
  

  
  
  return(cluster_intermediate)
  
  
  
  
  
}





