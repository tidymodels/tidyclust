#' Calculates Sum of Squared Error in each cluster
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#' cluster.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   within_cluster_sse()
#'
#' @export
within_cluster_sse <- function(object, new_data = NULL,
                               dist_fun = Rfast::dista) {


  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {
    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)
  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {
    res <- tibble::tibble(
      .cluster = factor(summ$cluster_names),
      wss = summ$within_sse
    )
  } else {
    dist_to_centroids <- dist_fun(summ$centroids, new_data)

    res <- dist_to_centroids %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      purrr::map_dfr(~ c(
        .cluster = which.min(.x),
        dist = min(.x)^2
      )) %>%
      dplyr::mutate(
        .cluster = factor(paste0("Cluster_", .cluster))
      ) %>%
      dplyr::group_by(.cluster) %>%
      dplyr::summarize(wss = sum(dist))
  }

  return(res)
}

#' Compute the sum of within-cluster SSE
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_wss()
#' @export
tot_wss <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
  sum(within_cluster_sse(object, new_data, dist_fun, ...)$wss, na.rm = TRUE)
}

#' Compute the total sum of squares
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_sse()
#' @export
tot_sse <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {


  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {
    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)
  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {
    tot <- summ$tot_sse
  } else {
    overall_mean <- colSums(summ$centroids * summ$n_members) / sum(summ$n_members)
    tot <- dist_fun(t(as.matrix(overall_mean)), new_data)^2 %>% sum()
  }

  return(tot)
}

#' Compute the ratio of the WSS to the total SSE
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   sse_ratio()
#' @export
sse_ratio <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
  tot_wss(object, new_data, dist_fun) / tot_sse(object, new_data, dist_fun)
}
