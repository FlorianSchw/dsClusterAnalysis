% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distDS.R
\name{distDS}
\alias{distDS}
\title{Computes the distance matrix of a given data set}
\usage{
distDS(df.name, method)
}
\arguments{
\item{df.name}{is a string character of the data set (either a data.frame or a matrix)}

\item{method}{specifies the method for the distance matrix calculation and can be either 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'}
}
\value{
the object specified by the \code{newobj} argument of \code{ds.dist} or default name \code{dist.newobj}
}
\description{
This function is similar to the native R function from stats
}
\details{
The function computes the distance matrix of a data set with multiple
variables.
}
\author{
Florian Schwarz for the German Institute of Human Nutrition
}
