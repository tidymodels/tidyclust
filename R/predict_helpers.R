stats_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$cluster)
  res <- apply(flexclust::dist2(object$centers[reorder_clusts, ], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}

clusterR_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$clusters)
  res <- apply(flexclust::dist2(object$centroids[reorder_clusts,], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}
