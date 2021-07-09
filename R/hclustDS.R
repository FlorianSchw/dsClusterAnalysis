#'
#' @title Conducts hierarchical clustering of a given dissimilarity structure
#' @description This function is similar to the native R function 'hclust' from stats
#' @details The function computes the hierarchical clustering of a dissimilarity structure (type 'dist'). 
#' @param diss is a string character of the dissimilarity structure
#' @param method specifies the method for the calculation of the hierarchical clustering and can be either 'ward.D', 'ward.D2', 'single', 
#' 'complete', 'average', 'mcquitty', 'median' or 'centroid'
#' @return the object specified by the \code{newobj} argument of \code{ds.hclust} or default name \code{hclust.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 


hclustDS <- function(diss, method){
  
  
  diss <- eval(parse(text=diss), envir = parent.frame())  
  

  # Computes the hierarchical clustering
  outcome <- stats::hclust(diss, method)  

  # Assigns the hierarchical clustering outcome to the server side  
  return(outcome)
 
  
}

# ASSIGN function
# hclustDS


