#' Extract cluster assignments from model
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_cluster_assignment()
#' @export
extract_cluster_assignment <- function(object, ...) {
  UseMethod("extract_cluster_assignment")
}

#' @export
extract_cluster_assignment.cluster_fit <- function(object, ...) {
  extract_cluster_assignment(object$fit)
}

#' @export
extract_cluster_assignment.kmeans <- function(object, ...) {
  cluster_assignment_tibble(object$cluster, length(object$size))
}

#' @export
extract_cluster_assignment.KMeansCluster <- function(object, ...) {
  cluster_assignment_tibble(object$clusters, length(object$obs_per_cluster))
}

# ------------------------------------------------------------------------------

cluster_assignment_tibble <- function(clusters, n_clusters) {

  reorder_clusts <- order(unique(clusters))
  names <- paste0("Cluster_", 1:n_clusters)
  res <- names[reorder_clusts][clusters]

  tibble::tibble(.cluster = factor(res))
}
