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

  #Copying data set
  final_dat <- dat

  #Number of rows/observations for later check
  original_length <- nrow(dat)
  num_obs <- nrow(dat)

  #Vectors of clusters
  last_clusters <- c(0)
  border_points <- c(0)

  #Counters
  iterations <- 0
  cluster <- 1


  repeat {

    #If on second round of cluster assignment, then subset original dataframe to take out observations that have been assigned clusters
    if(cluster > 1) {

      dat <- dat[!(dat$row_index %in% rows_to_exclude$row_index), ]
      num_obs <- nrow(dat)
      row_indices <- dat$row_index

    }

    #Randomly select 1 observations
    random_obs <- sample(num_obs, size = 1)

    #Create starting center for reference
    starting_center <- slice(dat, random_obs)

    #Reset distance and cluster_assignment vectors so they keep the right length
    dist <- c(0)
    cluster_assignment <- c(0)
    core_points <- c(0)

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

    #Add cluster assignments to final data set
    if(cluster == 1) {

      final_dat$clusters <- cluster_assignment

    } else {

      final_dat <- final_dat %>%
        mutate(clusters = replace(clusters, row_indices, cluster_assignment))

    }

    #Creating data frame of rows to exclude in next iteration
    rows_to_exclude <- final_dat %>%
      tidyr::drop_na() %>%
      select(row_index)

    #Add one to cluster to indicate next iteration of clustering
    cluster <- cluster + 1

    #If all of the cluster assignment has been
    if(nrow(dat) <= 1) {
      break
    }

  }

  return(final_dat)

}
