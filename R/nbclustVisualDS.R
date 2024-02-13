#'
#' @title Determines optimal number of clusters for dataset
#' @description This function is similar to the R function 'fviz_nbclust' from the factoextra package
#' @details The function uses partitioning methods to find optimal numbers of clusters for a given dataset.
#' @param df.name is a string character of the data set and can be either a matrix or data frame
#' @param FUNcluster is a partitioning function 
#' @param method the method to be used for estimating the optimal number of clusters. Possible values are "silhouette", "wss" and "gap_stat".
#' @param diss 'dist' object as produced by ds.dist. If diss = NULL, dist(df.name) is computed with the default "euclidean" setting.
#' @param k.max maximum number of clusters to be considered (has to be at least 2)
#' @param nboot number of Monte Carlo bootstrap samples. This argument is only used for determining the number of clusters for gap statistics
#' @param verbose logical values, If TRUE, the result of progress is printed. 
#' @param barfill fill color for bars
#' @param barcolor outline color for bars 
#' @param linecolor color for lines 
#' @param print.summary logical value. If TRUE, the optimal number of clusters are printed 
#' @return a ggplot2 image suggesting optimal number of clusters
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import factoextra 
#' @export
#' 




nbclustVisualDS <- function(df.name, FUNcluster, method, diss, k.max, nboot, verbose, barfill, barcolor, linecolor, print.summary){
  
  
  df.name <- eval(parse(text=df.name), envir = parent.frame())  
  
  
  # Computing k-means clustering of the data set
  if(identical(FUNcluster, "kmeans")){
   
  if(any(is.na(df.name))){
     stop("The data frames contains NAs.", call.=FALSE)
  }   
     
  nn <- dim(df.name)[1]
    
  if(k.max >= nn){
    stop("For this clustering method 'k.max' must be between 1 and nrow(df.name).", call.=FALSE)
  }
    
  result <- factoextra::fviz_nbclust(x = df.name, FUNcluster = stats::kmeans, method = method, diss = diss, k.max = k.max, nboot = nboot, verbose = verbose, barfill = barfill, barcolor = barcolor, linecolor = linecolor, print.summary = print.summary)  
  output <- result
  
  
  
  
  # Computing hierarchical clustering of the data set
  } else if(identical(FUNcluster, "hcut")){ 
  
  nn <- dim(df.name)[1]
    
  if(k.max >= nn){
    stop("For this clustering method 'k.max' must be between 1 and nrow(df.name).", call.=FALSE)
  }
    
  result <- factoextra::fviz_nbclust(x = df.name, FUNcluster = factoextra::hcut, method = method, diss = diss, k.max = k.max, nboot = nboot, verbose = verbose, barfill = barfill, barcolor = barcolor, linecolor = linecolor, print.summary = print.summary)  
  output <- result
    
  
  
 
 
  } else if(identical(FUNcluster, "cluster::pam")){ 
   
    
  # Computing cluster::pam clustering of the data set  
  nn <- dim(df.name)[1]
    
  if(k.max >= nn){
    stop("For this clustering method 'k.max' must be between 1 and nrow(df.name).", call.=FALSE)
  }
    
  result <- factoextra::fviz_nbclust(x = df.name, FUNcluster = cluster::pam, method = method, diss = diss, k.max = k.max, nboot = nboot, verbose = verbose, barfill = barfill, barcolor = barcolor, linecolor = linecolor, print.summary = print.summary)  
  output <- result
    
  
  
  
  # Computing cluster::clara clustering of the data set
  } else if(identical(FUNcluster, "cluster::clara")){ 
   
  if(any(is.na(df.name))){
    stop("The data frames contains NAs.", call.=FALSE)
  }   
     
  nn <- dim(df.name)[1]
    
  if(k.max >= nn){
    stop("For this clustering method 'k.max' must be between 1 and nrow(df.name).", call.=FALSE)
  }
    
  result <- factoextra::fviz_nbclust(x = df.name, FUNcluster = cluster::clara, method = method, diss = diss, k.max = k.max, nboot = nboot, verbose = verbose, barfill = barfill, barcolor = barcolor, linecolor = linecolor, print.summary = print.summary)  
  output <- result
    
  
  
  
  # Computing cluster::fanny clustering of the data set
  } else if(identical(FUNcluster, "cluster::fanny")){ 
    
  nn <- dim(df.name)[1]
    
  if(k.max > (nn/2 - 1)){
    stop("For this clustering method 'k.max' must be in 1,2, .., n/2 -1.", call.=FALSE)
  }
    
  result <- factoextra::fviz_nbclust(x = df.name, FUNcluster = cluster::fanny, method = method, diss = diss, k.max = k.max, nboot = nboot, verbose = verbose, barfill = barfill, barcolor = barcolor, linecolor = linecolor, print.summary = print.summary)  
  output <- result
    
  } 
   
  
  
  # Returning the outcome of potentially best number of clusters back to the analyst
  return(output)  
  
}

# AGGREGATE function
# nbclustVisualDS
