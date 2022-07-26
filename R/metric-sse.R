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
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
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
      map(~ c(
        .cluster = which.min(.x),
        dist = min(.x)^2
      )) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        .cluster = factor(paste0("Cluster_", .cluster))
      ) %>%
      dplyr::group_by(.cluster) %>%
      dplyr::summarize(wss = sum(dist),
                       n_obs = n())
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
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_wss()
#'
#' kmeans_fit %>%
#'   tot_wss_vec()
#' @export
tot_wss <- function(object, ...) {
  UseMethod("tot_wss")
}

tot_wss <- new_cluster_metric(
  tot_wss,
  direction = "zero"
)

#' @export
#' @rdname tot_wss
tot_wss.cluster_fit <- function(object, new_data = NULL,
                                dist_fun = NULL, ...) {

  if (is.null(dist_fun)) {
    dist_fun <- Rfast::dista
  }

  res <- tot_wss_impl(object, new_data, dist_fun, ...)

  tibble::tibble(
    .metric = "tot_wss",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname tot_wss
tot_wss.workflow <- tot_wss.cluster_fit

#' @export
#' @rdname tot_wss
tot_wss_vec <- function(object, new_data = NULL,
                                dist_fun = Rfast::dista, ...) {
  tot_wss_impl(object, new_data, dist_fun, ...)
}

tot_wss_impl <- function(object, new_data = NULL,
                                dist_fun = Rfast::dista, ...) {
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
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_sse()
#'
#' kmeans_fit %>%
#'   tot_sse_vec()
#' @export
tot_sse <- function(object, ...) {
  UseMethod("tot_sse")
}

tot_sse <- new_cluster_metric(
  tot_sse,
  direction = "zero"
)

#' @export
#' @rdname tot_sse
tot_sse.cluster_fit <- function(object, new_data = NULL,
                                dist_fun = NULL, ...) {
  if (is.null(dist_fun)) {
    dist_fun <- Rfast::dista
  }

  res <- tot_sse_impl(object, new_data, dist_fun, ...)

  tibble::tibble(
    .metric = "tot_sse",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname tot_sse
tot_sse.workflow <- tot_sse.cluster_fit

#' @export
#' @rdname tot_sse
tot_sse_vec <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
  tot_sse_impl(object, new_data, dist_fun, ...)
}

tot_sse_impl <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
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
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   sse_ratio()
#' @export
sse_ratio <- function(object, ...) {
  UseMethod("sse_ratio")
}

sse_ratio <- new_cluster_metric(
  sse_ratio,
  direction = "zero"
)

#' @export
#' @rdname sse_ratio
sse_ratio.cluster_fit <- function(object, new_data = NULL,
                                dist_fun = NULL, ...) {
  if (is.null(dist_fun)) {
    dist_fun <- Rfast::dista
  }
  res <- sse_ratio_impl(object, new_data, dist_fun, ...)

  tibble::tibble(
    .metric = "sse_ratio",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname sse_ratio
sse_ratio.workflow <- sse_ratio.cluster_fit

#' @export
#' @rdname sse_ratio
sse_ratio_vec <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
  sse_ratio_impl(object, new_data, dist_fun, ...)
}


sse_ratio_impl <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {
  tot_wss_vec(object, new_data, dist_fun) /
    tot_sse_vec(object, new_data, dist_fun)
}
