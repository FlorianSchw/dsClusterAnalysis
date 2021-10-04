#'
#' @title Determines optimal number of clusters for dataset
#' @description This function is similar to the R function 'NBClust' from the NBClust package
#' @details The function uses partitioning methods to find optimal numbers of clusters for a given dataset.
#' @param df.name is a string character of the data set and can be either a matrix or data frame
#' @param diss is a dissimilarity structure which will be calculated according to the distance method
#' @param distance specifies the method for the distance matrix calculation and can be either 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'
#' @param min.nc specifies the minimum number of clusters
#' @param max.nc specifies the maximum number of clusters
#' @param method describes the clustering method and can be either "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans" or "ward.D"
#' @param index describes the clustering index and can be either "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" or "alllong"         
#' @param alphaBeale value for "beale" clustering index
#' @param seed is an integer number for random start point
#' @return a ggplot2 image suggesting optimal number of clusters
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @importFrom NbClust NbClust
#' @export
#' 


nbclustDS <- function(df.name, diss, distance, min.nc, max.nc, method, index, alphaBeale, seed){
  
  
  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  
  
  # Computing k-means clustering of the data set
  set.seed(seed)
  result <- NbClust::NbClust(data = df.name, diss = diss, distance = distance, min.nc = min.nc, max.nc = max.nc, method = method, index = index, alphaBeale = alphaBeale)  
  output <- result
    
  
  # Assigning the k-means clustering object to the server-side
  return(output)  
  
  
}

# AGGREGATE function
# nbclustDS

