stats_kmeans_predict <- function(object, new_data) {
  res <- apply(flexclust::dist2(object$centers, new_data), 2, which.min)
  factor(res, levels = seq_along(object$size))
}
