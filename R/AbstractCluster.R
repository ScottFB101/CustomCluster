#' Implements
#'
#' @param dat A data set
#' @param k The number of desired clusters
#'
#' @return Returns cluster assignments
#'
#' @import dplyr
#' @import stats
#'
#' @export

AbstractCluster <- function(dat, distance = 0.1, neighbors = 5) {

  #Taking only numeric data
  dat <- dat %>%
    select(. ,where(is.numeric)) %>%
    mutate_all(scale)

  #Number of rows/observations
  num_obs <- nrow(dat)

  #Randomly select 1 observations
  random_obs <- sample(num_obs, size = 1)

  #Create centers for reference
  starting_center <- slice(dat, random_obs)

  #Vectors of clusters
  clusters <- c(1)
  last_clusters <- c(0)
  smallest_distances <- c(0)
  dist <- c(0)
  iterations <- 0

  #1. Pick a random point
  #2. See what points are within distance declared in function
  #3. Add them to the cluster based on being within that distance
  #4. For loop to check if the points just added in that cluster have enough "neighbor" points within that distance
  #5. If so, add them to cluster





  }


}

neighbour_distance <- function(dat, k) {







}
