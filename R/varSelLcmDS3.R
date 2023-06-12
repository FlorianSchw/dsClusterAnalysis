#'
#' @export
#' 


varSelLcmDS3 <- function(newobj_final, num.clust){
  
  

  
  cluster_pre <- unlist(eval(parse(text="cluster_pre"), envir = parent.frame()))  
  ClusterMatching <- eval(parse(text="ClusterMatching"), envir = parent.frame())  
  
  
  
  cluster_post <- c()
  for (r in 1:length(cluster_pre)){
    
    cluster_post[r] <- ClusterMatching$Matching[which(ClusterMatching$Cluster == cluster_pre[r])]
    
  }
  
  
  return(cluster_post)
  
}



