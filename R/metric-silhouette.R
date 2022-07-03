#' Measures silhouettes between clusters
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dists A distance matrix. Used if `new_data` is `NULL`.
#' @param dist_fun A function for calculating distances between observations.
#'   Defaults to Euclidean distance on processed data.
#'
#' @return A tibble giving the silhouettes for each observation.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars %>%
#'   as.matrix() %>%
#'   dist()
#'
#' silhouettes(kmeans_fit, dists = dists)
#' @export
silhouettes <- function(object, new_data = NULL, dists = NULL,
                        dist_fun = Rfast::Dist) {
  preproc <- prep_data_dist(object, new_data, dists, dist_fun)

  clust_int <- as.integer(gsub("Cluster_", "", preproc$clusters))

  sil <- cluster::silhouette(clust_int, preproc$dists)

  sil %>%
    unclass() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      cluster = factor(paste0("Cluster_", cluster)),
      neighbor = factor(paste0("Cluster_", neighbor)),
      sil_width = as.numeric(sil_width)
    )
}

#' Measures average silhouette across all observations
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dists A distance matrix. Used if `new_data` is `NULL`.
#' @param dist_fun A function for calculating distances between observations.
#'   Defaults to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A double; the average silhouette.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars %>%
#'   as.matrix() %>%
#'   dist()
#'
#' avg_silhouette(kmeans_fit, dists = dists)
#' @export
avg_silhouette <- function(object, new_data = NULL, dists = NULL,
                           dist_fun = Rfast::Dist, ...) {
  mean(silhouettes(object, new_data, dists, dist_fun, ...)$sil_width)
}
