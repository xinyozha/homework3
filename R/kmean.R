my_kmeans <- function(data, k, max_iter = 100) {
  n <- nrow(data)
  dimensions <- ncol(data)

  # Choose the center
  centers <- data[sample(1:n, k), ]
  clusters <- numeric(n)
  iter <- 0

  repeat {
    iter <- iter + 1

    for (i in 1:n) {
      distances <- apply(centers, 1, function(center) sum((data[i, ] - center)^2))
      clusters[i] <- which.min(distances)
    }

    # update center
    new_centers <- matrix(NA, nrow = k, ncol = dimensions)
    for (j in 1:k) {
      cluster_points <- data[clusters == j, ]
      if (nrow(cluster_points) > 0) {
        new_centers[j, ] <- colMeans(cluster_points)
      } else {
        new_centers[j, ] <- centers[j, ]
      }
    }

    # check
    if (all(new_centers == centers) || iter >= max_iter) {
      break
    }
    centers <- new_centers
  }

  return(list(centers = centers, clusters = clusters))
}
