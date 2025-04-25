#' Measures silhouette between clusters
#'
#' @param object A fitted tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dists A distance matrix. Used if `new_data` is `NULL`.
#' @param dist_fun A function for calculating distances between observations.
#'   Defaults to Euclidean distance on processed data.
#'
#' @details [silhouette_avg()] is the corresponding cluster metric function that
#' returns the average of the values given by `silhouette()`.
#'
#' @return A tibble giving the silhouette for each observation.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars |>
#'   as.matrix() |>
#'   dist()
#'
#' silhouette(kmeans_fit, dists = dists)
#' @export
silhouette <- function(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance
) {
  if (inherits(object, "cluster_spec")) {
    cli::cli_abort(
      c(
        "This function requires a fitted model.",
        "i" = "Please use {.fn fit} on your cluster specification."
      )
    )
  }

  preproc <- prep_data_dist(object, new_data, dists, dist_fun)

  clust_int <- as.integer(gsub("Cluster_", "", preproc$clusters))

  sil <- cluster::silhouette(clust_int, preproc$dists)

  if (!inherits(sil, "silhouette")) {
    res <- tibble::tibble(
      cluster = preproc$clusters,
      neighbor = factor(
        rep(NA_character_, length(preproc$clusters)),
        levels = levels(preproc$clusters)
      ),
      sil_width = NA_real_
    )
    return(res)
  }

  sil |>
    unclass() |>
    tibble::as_tibble() |>
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
#' @details Not to be confused with [silhouette()] that returns a tibble
#'   with silhouette for each observation.
#'
#' @family cluster metric
#'
#' @return A double; the average silhouette.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars |>
#'   as.matrix() |>
#'   dist()
#'
#' silhouette_avg(kmeans_fit, dists = dists)
#'
#' silhouette_avg_vec(kmeans_fit, dists = dists)
#' @export
silhouette_avg <- function(object, ...) {
  UseMethod("silhouette_avg")
}

silhouette_avg <- new_cluster_metric(
  silhouette_avg,
  direction = "zero"
)

#' @export
#' @rdname silhouette_avg
silhouette_avg.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname silhouette_avg
silhouette_avg.cluster_fit <- function(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = NULL,
  ...
) {
  if (is.null(dist_fun)) {
    dist_fun <- philentropy::distance
  }

  res <- silhouette_avg_impl(object, new_data, dists, dist_fun, ...)

  tibble::tibble(
    .metric = "silhouette_avg",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname silhouette_avg
silhouette_avg.workflow <- silhouette_avg.cluster_fit

#' @export
#' @rdname silhouette_avg
silhouette_avg_vec <- function(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance,
  ...
) {
  silhouette_avg_impl(object, new_data, dists, dist_fun, ...)
}

silhouette_avg_impl <- function(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance,
  ...
) {
  mean(silhouette(object, new_data, dists, dist_fun, ...)$sil_width)
}
