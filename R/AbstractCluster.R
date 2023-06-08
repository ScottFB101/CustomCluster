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

AbstractCluster <- function(dat, distance = 3, neighbors = 5) {

  #Taking only numeric data
  dat <- dat %>%
    select(. ,where(is.numeric)) %>%
    mutate_all(scale) %>%
    tibble::rowid_to_column("row_index")

  #Number of rows/observations
  num_obs <- nrow(dat)

  #Vectors of clusters
  last_clusters <- c(0)
  border_points <- c(0)
  dist <- c(0)
  cluster_assignment <- c(0)

  #Counters
  iterations <- 0
  cluster <- 1

  #1. Pick a random point
  #2. See what points are within distance declared in function
  #3. Add them to the cluster based on being within that distance
  #4. For loop to check if the points just added in that cluster have enough "neighbor" points within that distance
  #5. If so, add them to cluster

  #Original_data[sub_dat$row_index, "cluster"]
  #Keep sub-setting data throughout the loop

  repeat {

    #If on second round of cluster assignment, then subset original dataframe to take out observations that have been assigned clusters
    if(cluster > 1) {

      dat <- dat[sub_dat$row_index, ]
      num_obs <- nrow(dat)

    }

    #Randomly select 1 observations
    random_obs <- sample(num_obs, size = 1)

    #Create starting center for reference
    starting_center <- slice(dat, random_obs)

    #Loop to calculate Euclidean distance of each point from the randomly selected centers
    for(i in 1:num_obs) {

      #Attaching observation to data frame of the randomly chosen center
      center_and_point <- dat[i, ] %>% rbind(starting_center)

      #Measuring Euclidean distance between observation and randomly chosen center
      dist[i] <- dist(center_and_point, method = "euclidean", upper = FALSE)

    }

    #Test if distance between points and center_point is within distance declared by user
    core_points <- (dist < distance)

    #If TRUE, then assign those observation's row value to the cluster
    cluster_assignment[core_points == TRUE] <- cluster

    #Add cluster assignments to original data set
    dat <- dat %>%
      cbind(cluster_assignment)

    #Creating
    browser()
    sub_dat <- dat[is.na(dat$cluster_assignment), row_index]

    #Add one to cluster to indicate next iteration of clustering
    cluster <- cluster + 1

    #If all of the cluster assignment has been
    if(all(is.na(dat$cluster_assignment)) == TRUE) {
      break
    }

  }

  return(dat)

}
