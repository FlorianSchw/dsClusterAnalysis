#'
#' @title Needs editing
#' @description Needs editing
#' @details Needs editing
#' @param num.clust specifies the number of clusters for the computation 
#' @param final_match_char needs editing
#' @param final_colnames_char needs editing
#' @export
#' 


varSelLcmDS4 <- function(num.clust, final_match_char, final_colnames_char = c("results_values", "Matching")){
  
  #df <- eval(parse(text="D"), envir = parent.frame())  
  
  
  final_match_char_vect <- as.numeric(unlist(strsplit(final_match_char, split = ",")))
  final_colnames <- unlist(strsplit(final_colnames_char, split = ","))
  
  final_match <- as.data.frame(matrix(data = final_match_char_vect, nrow = num.clust))
  colnames(final_match) <- final_colnames
  
  cluster_pre <- eval(parse(text="cluster_pre"), envir = parent.frame())[[1]]  
  
  
  cluster_post <- c()
  for (r in 1:length(cluster_pre)){
    
    cluster_post[r] <- final_match$Matching[which(final_match$results_values == cluster_pre[r])]
    
  }
  
  
  #extra_obj <- data.frame(df, cluster_pre, cluster_post)
  
  return(cluster_post)
  
}

