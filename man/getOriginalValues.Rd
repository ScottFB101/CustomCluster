% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AbstractCluster.R
\name{getOriginalValues}
\alias{getOriginalValues}
\title{Helper function for CreateElbowGraph that grabs the original data values from the indices provided for each datapoin in every cluster}
\usage{
getOriginalValues(indices, elbow_dat, explanatory, response)
}
\arguments{
\item{indices}{A vector holding the indices of a cluster's points}

\item{elbow_dat}{Original data frame}

\item{explanatory}{Explanatory variable of interest}

\item{response}{Response variable of interest}
}
\value{
A data frame that represents the true values of each point in a cluster
}
\description{
Helper function for CreateElbowGraph that grabs the original data values from the indices provided for each datapoin in every cluster
}
