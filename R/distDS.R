#'
#' @title Computes the distance matrix of a given data set
#' @description This function is similar to the native R function from stats
#' @details The function computes the distance matrix of a data set with multiple
#' variables.
#' @param df.name is a string character of the data set (either a data.frame or a matrix)
#' @param method specifies the method for the distance matrix calculation and can be either 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'
#' @return the object specified by the \code{newobj} argument of \code{ds.dist} or default name \code{dist.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 



distDS <- function(df.name, method){
  
  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  
   
  # Computes the distance matrix
  outcome <- stats::dist(df.name, method)

  # the outcome of the distance matrix is assigned to the data servers  
  return(outcome)  
  

  
}

# ASSIGN Function
# distDS













