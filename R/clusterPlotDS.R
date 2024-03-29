#'
#' @title Plots hierarchical cluster
#' @description This function is similar to the R function 'fviz_dend' from the factoextra package
#' @details The function computes the dendrogram without any labels to prevent any disclosures.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param tree is a string character of the data set
#' @param k specifies the number of clusters in which the tree should be cut 
#' @param h specifies the height of a tree at which the tree should be cut. If both, k and h, are given, k overrides h.
#' @param k_colors is a vector containing colors to be used for groups
#' @param palette is a vector containing colors
#' @param show_labels is a logical whether labels are shown in the plot. Always set to FALSE in the server side function 
#' @param color_labels_by_k is a logical value which colors the branches by group when k is not NULL
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @importFrom factoextra fviz_dend
#' @export
#' 




clusterPlotDS <- function(tree, k, h, k_colors, palette, show_labels = FALSE, color_labels_by_k){
  
  
  tree <- eval(parse(text=tree), envir = parent.frame())  
  
  # Computing k-means clustering of the data set
  result <- factoextra::fviz_dend(x = tree, 
                                  k = k, 
                                  h = h, 
                                  k_colors = k_colors, 
                                  palette = palette, 
                                  show_labels = FALSE, 
                                  color_labels_by_k = color_labels_by_k)  
  output <- result
  
  # Assigning the k-means clustering object to the server-side
  return(output)  
  
  
  
}

# AGGREGATE function
# clusterPlotDS








