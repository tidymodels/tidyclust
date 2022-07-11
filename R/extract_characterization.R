#' Extract clusters from model
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' set.seed(1234)
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_centroids()
#' @export
extract_centroids <- function(object, ...) {
  summ <- extract_fit_summary(object)
  clusters <- tibble::tibble(.cluster = summ$cluster_names)
  bind_cols(clusters, summ$centroids)
}
