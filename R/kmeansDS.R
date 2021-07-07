#'
#' @title Conducts k-means clustering of a given data set
#' @description This function is similar to the native R function 'kmeans' from stats
#' @details The function calls the server-side function \code{kmeansDS} that computes the
#' k-means clustering of a data set (type 'data.frame' or 'matrix'). 
#' The function creates a new object on the server-side, which is of class 'kmeans'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param df.name is a string character of the data set.  The \code{clusters} argument specifies the number of clusters
#' for the computation of the clustering.
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 




kmeansDS <- function(df.name, clusters, iter.max, nstart, algorithm){


# Computing k-means clustering of the data set
outcome <- stats::kmeans(df.name, centers = clusters, iter.max = iter.max, nstart = nstart, algorithm = algorithm)  
  

# Assigning the k-means clustering object to the server-side
return(outcome)  
  
  
  
}

# ASSIGN funtion
# kmeansDS




















