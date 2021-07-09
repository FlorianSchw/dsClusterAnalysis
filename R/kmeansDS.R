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
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 




kmeansDS <- function(df.name, clusters, iter.max, nstart, algorithm, trace = FALSE){


  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  algorithm <- eval(parse(text=algorithm), envir = parent.frame())
  
  # Computing k-means clustering of the data set
  outcome <- stats::kmeans(df.name, clusters, iter.max, nstart, algorithm, trace = FALSE)  
  

  # Assigning the k-means clustering object to the server-side
  return(outcome)  
  
  
  
}

# ASSIGN funtion
# kmeansDS




















