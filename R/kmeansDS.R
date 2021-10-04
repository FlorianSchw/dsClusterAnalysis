#'
#' @title Conducts k-means clustering of a given data set
#' @description This function is similar to the native R function 'kmeans' from stats
#' @details The function calls the server-side function \code{kmeansDS} that computes the
#' k-means clustering of a data set (type 'data.frame' or 'matrix'). 
#' The function creates a new object on the server-side, which is of class 'kmeans'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param df.name is a string character of the data set
#' @param clusters specifies the number of clusters for the computation 
#' @param iter.max specifies the max. number of iterations allowed
#' @param nstart relates to the number of random sets if clusters is a number and not a set of initial cluster centers
#' @param algorithm refers to the algorithm of calculating the kmeans and can be either 'Hartigan-Wong', 'Lloyd', 'Forgy' or 'MacQueen' 
#' @param trace logical or Integer number tracing information on the progress of the algorithm
#' @param seed is an integer for setSeed
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 




kmeansDS <- function(df.name, clusters, iter.max, nstart, algorithm, trace = FALSE, seed){


  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  
  
 # check for NA objects
   if(any(is.na(df.name))){
    stop("The data frames contains NAs.", call.=FALSE)
   }  
  
  
  # Computing k-means clustering of the data set
  set.seed(seed)
  result <- stats::kmeans(df.name, clusters, iter.max, nstart, algorithm, trace = FALSE)  
  output <- result[[1]]

  # Assigning the k-means clustering object to the server-side
  return(output)  
  
  
  
}

# ASSIGN funtion
# kmeansDS

