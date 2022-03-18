#' Extract clusters from model
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' set.seed(1234)
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_clusters()
#' @export
extract_clusters <- function(object, ...) {
  UseMethod("extract_clusters")
}

#' @export
extract_clusters.cluster_fit <- function(object, ...) {
  extract_clusters(object$fit)
}

#' @export
extract_clusters.kmeans <- function(object, ...) {
  clusters_tibble(object$centers)
}

#' @export
extract_clusters.KMeansCluster <- function(object, ...) {
  clusters_tibble(object$centroids)
}

# ------------------------------------------------------------------------------

clusters_tibble <- function(x) {
  clusters <- tibble::tibble(.cluster = factor(seq_len(nrow(x))))

  bind_cols(clusters, x)
}
