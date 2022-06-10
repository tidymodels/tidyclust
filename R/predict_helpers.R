stats_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$cluster)
  res <- apply(flexclust::dist2(object$centers[reorder_clusts, , drop = FALSE], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}

clusterR_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$clusters)
  res <- apply(flexclust::dist2(object$centroids[reorder_clusts, , drop = FALSE], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}

stats_hier_clust_predict <- function(object, new_data) {
  ### record joins of new data according to h
  ### this means we need an h even if k was specified (min or max or half?)
  ### min because if they joined above, then that cut would be more clusts

  ### find closest from original data, using specified linkage

}
