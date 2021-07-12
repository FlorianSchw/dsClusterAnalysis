#'
#' @title Conducts k-means clustering of a given data set
#' @description This function is similar to the native R function 'kmeans' from stats
#' @details The function calls the server-side function \code{kmeansDS} that computes the
#' k-means clustering of a data set (type 'data.frame' or 'matrix'). 
#' The function creates a new object on the server-side, which is of class 'kmeans'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param df.name is a string character of the data set
#' @param k specifies the number of clusters in which the tree should be cut 
#' @param h specifies the height of a tree at which the tree should be cut
#' @param k_colors is a vector containing colors to be used for groups
#' @param color_labels_by_k is a logical value which colors the branches by group when k is not NULL
#' @param rect is a logical value which specifies whether to add a rectangle around groups when k is not NULL
#' @param main is the main title of the plot 
#' @param xlab is a title for the x axis 
#' @param ylab is a title for the y axis 
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import factoextra 
#' @export
#' 




clusterPlotDS <- function(df.name, k, h, k_colors, color_labels_by_k, rect, main, xlab, ylab){
  
  
  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  
  
  # Computing k-means clustering of the data set
  result <- factoextra::fviz_dend(df.name, k, h, k_colors, show_labels = FALSE, rect, color_labels_by_k, main, xlab, ylab)  
  output <- result
  
  # Assigning the k-means clustering object to the server-side
  return(output)  
  
  
  
}

# AGGREGATE funtion
# clusterPlotDS








