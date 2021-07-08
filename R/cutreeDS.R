#'
#' @title Cuts the tree of a hclust object
#' @description This function is similar to the native R function from stats
#' @details The function computes the clusters for a given number of clusters k or height h of the tree, and assigns the new object to the server-side.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{cutree.newobj} by default.
#' @param tree is a string character specifying the name of the hclust object 
#' @param k specifies the number of clusters in which the tree should be cut
#' @param h specifies the height of a tree at which the tree should be cut
#' @return the object specified by the \code{newobj} argument of \code{ds.cutree} or default name \code{cutree.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 


cutreeDS <- function(tree, k = NULL, h = NULL){
  

  # Cuts the tree
  outcome <- stats::cutree(tree = tree, k = k, h = h)  
  
  # Assigns the resulting vector with the cluster numbers to the server side  
  return(outcome)
  
  
  
}























