stats_kmeans_predict <- function(object, new_data) {
  orig_labels_ordered <- unique(object$cluster)  # reorder centers
  centroids <- object$centers
  clusters <- apply(flexclust::dist2(centroids, new_data), 2, which.min)

  relabel_clusters(clusters, orig_labels_ordered, nrow(centroids))
}

clusterR_kmeans_predict <- function(object, new_data) {
  orig_labels_ordered <- unique(object$clusters)
  centroids <- object$centroids  # reorder centers
  clusters <- apply(flexclust::dist2(centroids, new_data), 2, which.min)

  relabel_clusters(clusters, orig_labels_ordered, nrow(centroids))
}

relabel_clusters <- function(clusters, orig_labels_ordered, n_clusters) {

  factor(clusters,
         levels = orig_labels_ordered,
         labels = paste0("Cluster_", 1:n_clusters))

}
