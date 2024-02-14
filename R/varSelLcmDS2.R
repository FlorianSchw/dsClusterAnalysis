#'
#' @import VarSelLCM
#' @import dplyr
#' @import truncnorm
#' @importFrom stringr regex
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @export
#' 


varSelLcmDS2 <- function(df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations, initialRun_char_vect, colnames_char_vect, entries_per_study){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  
  pre_information_vector <- as.numeric(unlist(strsplit(initialRun_char_vect, split = ",")))
  pre_information_colnames <- unlist(strsplit(colnames_char_vect, split = ","))
  
  
  analysed_studies <- length(pre_information_vector) / (length(pre_information_colnames) * num.clust)
  df_cells <- length(pre_information_colnames) * num.clust
  
  #### Analyse which of the summary tables is equal to the study sites df, in order for it to be excluded
  for (a in seq_along(1:analysed_studies)){
    
    information_study <- as.data.frame(matrix(data = pre_information_vector[(1 + (a - 1)* df_cells):(df_cells* a)], nrow = num.clust))
    colnames(information_study) <- pre_information_colnames
    
    
    variables_of_interest <- colnames(information_study)[which(!(grepl("results_|Observations", colnames(information_study))))]
    variable_columns <- strsplit(x = variables_of_interest, split =  "_X_", fixed = TRUE)
    
    
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
    
    
    Test_Obj_Drop_Add1 <- information_study %>%
      select(starts_with("Mean_X_")) %>%
      rename_with(~ stringr::str_replace(., regex("^Mean_X_", ignore_case = TRUE), ""))%>%
      mutate(across(everything(), ~.x * information_study$Observations)) %>%
      summarise(across(everything(), sum)) %>%
      mutate(across(everything(), ~.x / sum(information_study$Observations)))
    
    Test_Obj_Drop_Add2 <- information_study %>%
      select(starts_with("CAT_X_")) %>%
      rename_with(~ stringr::str_replace(., regex("^CAT_X_", ignore_case = TRUE), ""))%>%
      summarise(across(everything(), sum)) %>%
      mutate(sweep(across(everything()), 2, as.numeric(t(categ$expression)), "*")) %>%
      select(starts_with(variable_cat)) %>%
      pivot_longer(cols = everything()) %>%
      mutate(name = sub("_X_.*", "", name)) %>%
      group_by(name) %>%
      summarise(value = sum(value, na.rm = FALSE)) %>%
      pivot_wider() %>%
      mutate(across(everything(), ~.x / sum(information_study$Observations)))
    
    Test_Obj_Drop_Add_Complete <- data.frame(Test_Obj_Drop_Add1, 
                                             Test_Obj_Drop_Add2)
    
    
    variable_all <- c(variable_cat,
                      variable_cont)
    
    variables_in_df <- colnames(df)
    
    for (i in 1:length(variable_all)){
      if(!(variable_all[i] %in% colnames(df))){
        
        dummy <- as.numeric(c(rep(NA, dim(df)[1])))
        df$dummy <- dummy
        names(df)[names(df) == "dummy"] <- variable_all[i]
        
      }
    }
    
    
    Test_Obj_Drop_Original <- df %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(where(is.character), as.numeric)) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
    
    
    Test_Obj_Drop_Original <- Test_Obj_Drop_Original[, order(colnames(Test_Obj_Drop_Original))]
    Test_Obj_Drop_Add_Complete <- Test_Obj_Drop_Add_Complete[, order(colnames(Test_Obj_Drop_Add_Complete))]
    
    
    current_information_equal <- all.equal(Test_Obj_Drop_Add_Complete, 
                                           Test_Obj_Drop_Original)
    
    
    if(identical(current_information_equal, TRUE)){
      
      message("Duplicate Dataset Identified.")
      
    } else {
      
      assign(paste0("StudyDataCreatedForClusteringObj", a), information_study)
      
    }
    
  }
  
  
  
  list_other_study_data <- objects(pattern = "StudyDataCreatedForClusteringObj")
  
  
  assignments <- list()
  summaries <- list()
  store_dfs <- list()
  
  check_df_total <- list()
  
  for (ww in 1:num.iterations){
    
    
    extension_dfs <- list()
    for (qqq in 1:length(list_other_study_data)){
      
      data_structure <- eval(parse(text=list_other_study_data[qqq]))  
      
      zzz <- list()
      data_extension <- df[1:sum(data_structure$Observations),]
      data_extension[,] <-  NA
      
      
      for (p in 1:length(variable_cont)){
        
        for (j in 1:dim(data_structure)[1]) {
          
          
          mean <- data_structure[[paste0("Mean_X_",variable_cont[p])]][j]
          sd <- data_structure[[paste0("SD_X_",variable_cont[p])]][j]
          length_1 <- data_structure$Observations[j]
          
          if(!(all(is.na(data_structure[[paste0("Mean_X_",variable_cont[p])]])))){
            
            
            if(mean - 2*sd < 0){
              var_min <- 0
            } else {
              var_min <- mean - 2*sd
            }
            
            var_max <- mean + 2*sd
            
            zzz[[j]] <- rtruncnorm(length_1, mean = mean, sd = sd, a = var_min, b = var_max)
            
          } 
          
        }
        
        tmp_collect <- unlist(zzz)
        
        if(!(all(is.na(data_structure[[paste0("Mean_X_",variable_cont[p])]])))){
          data_extension[[variable_cont[p]]] <- tmp_collect
        }
      }
      
      
      
      for (p in 1:length(variable_cat)){
        
        current_cat_variable <- data_structure[which(grepl(paste0("_X_" , variable_cat[p], "_X_"), colnames(data_structure), fixed = TRUE))]
        
        if(!(all(is.na(current_cat_variable)))){
          
          
          
          uuu <- list()
          
          for (j in 1:dim(data_structure)[1]){
            
            lll <- list()
            
            for (k in 1:dim(current_cat_variable)[2]){
              
              lll[[k]] <- rep(as.numeric(strsplit(colnames(current_cat_variable[k]), "_X_")[[1]][3]), current_cat_variable[j,k])
              
            } 
            
            tmp_cat <- sample(unlist(lll))
            uuu[[j]] <- tmp_cat
            
          }
        }
        
        
        if(!(all(is.na(current_cat_variable)))){
          
          tmp_coll <- unlist(uuu)
          data_extension[[variable_cat[p]]] <- as.factor(tmp_coll)
          
        }
        
        if((all(is.na(current_cat_variable)))){
          
          data_extension[[variable_cat[p]]] <- as.factor(c(rep(NA, sum(data_structure$Observations))))
          
        }
        
        
        
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
  
  dataframe_pooled <- dataframe_pooled %>%
    mutate(across(where(is.character), as.factor)) 
  
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
  results_values <- results_values_final[1:nrow(df)]
  
  
  outcome <- list(results_values,
                  FinalResults)
  
  
  
  return(outcome)
  
  
  
  
  
}