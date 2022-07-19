#' Extract cluster assignments from model
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
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
  extract_cluster_assignment(object$fit, ...)
}

#' @export
extract_cluster_assignment.workflow <- function(object, ...) {
  extract_cluster_assignment(object$fit$fit$fit)
}

#' @export
extract_cluster_assignment.kmeans <- function(object, ...) {
  cluster_assignment_tibble(object$cluster, length(object$size))
}

#' @export
extract_cluster_assignment.KMeansCluster <- function(object, ...) {
  cluster_assignment_tibble(object$clusters, length(object$obs_per_cluster))
}

#' @export
extract_cluster_assignment.hclust <- function(object, ...) {

  # if k or h is passed in the dots, use those.  Otherwise, use attributes
  # from original model specification
  args <- list(...)
  if (!("k" %in% names(args) | "cut_height" %in% names(args))) {
    k <- attr(object, "k")
    cut_height <- attr(object, "cut_height")
  }
  clusters <- stats::cutree(object, k, h = cut_height)
  cluster_assignment_tibble(clusters, length(unique(clusters)))
}

# ------------------------------------------------------------------------------

cluster_assignment_tibble <- function(clusters, n_clusters) {
  reorder_clusts <- order(unique(clusters))
  names <- paste0("Cluster_", 1:n_clusters)
  res <- names[reorder_clusts][clusters]

  tibble::tibble(.cluster = factor(res))
}
