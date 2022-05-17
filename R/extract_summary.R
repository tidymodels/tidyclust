
#' S3 method to get fitted model summary info depending on engine
#'
#' @param object a fitted cluster_spec object
#' @param ... other arguments passed to methods
#'
#' @return A list with various summary elements
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_fit_summary()
#' @export
extract_fit_summary <- function(object, ...) {
  UseMethod("extract_fit_summary")
}

#' @export
extract_fit_summary.cluster_fit <- function(object, ...) {
  extract_fit_summary(object$fit)
}

#' @export
extract_fit_summary.workflow <- function(object, ...) {
  extract_fit_summary(object$fit$fit$fit)
}

#' @export
extract_fit_summary.kmeans <- function(object, ...) {

  reorder_clusts <- order(unique(object$cluster))

  list(
    cluster_names = paste0("Cluster_", 1:nrow(object$centers)),
    centroids = object$centers[reorder_clusts],
    n_members = object$size[reorder_clusts],
    within_sse = object$withinss[reorder_clusts],
    tot_sse = object$totss,
    orig_labels = object$cluster
  )

}

#' @export
extract_fit_summary.KMeansCluster <- function(object, ...) {

  reorder_clusts <- order(unique(object$cluster))

  list(
    cluster_names = paste0("Cluster_", 1:nrow(object$centroids)),
    centroids = object$centroids[reorder_clusts,],
    n_members = object$obs_per_cluster[reorder_clusts],
    within_sse = object$WCSS_per_cluster[reorder_clusts],
    tot_sse = object$total_SSE,
    orig_labels = object$clusters
  )
}
