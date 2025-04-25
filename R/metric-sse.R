#' Calculates Sum of Squared Error in each cluster
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#'   to Euclidean distance on processed data.
#'
#' @details [sse_within_total()] is the corresponding cluster metric function
#' that returns the sum of the values given by `sse_within()`.
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#'   cluster.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' sse_within(kmeans_fit)
#' @export
sse_within <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  }
) {
  if (inherits(object, "cluster_spec")) {
    cli::cli_abort(
      c(
        "This function requires a fitted model.",
        "i" = "Please use {.fn fit} on your cluster specification."
      )
    )
  }

  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {
    new_data <- extract_post_preprocessor(object, new_data)
  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {
    res <- tibble::tibble(
      .cluster = factor(summ$cluster_names),
      wss = summ$sse_within_total_total,
      n_members = summ$n_members
    )
  } else {
    suppressMessages(
      dist_to_centroids <- dist_fun(
        as.matrix(summ$centroids),
        as.matrix(new_data)
      )
    )

    res <- dist_to_centroids |>
      tibble::as_tibble(.name_repair = "minimal") |>
      map(
        \(.x)
          c(
            .cluster = which.min(.x),
            dist = min(.x)^2
          )
      ) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        .cluster = factor(paste0("Cluster_", .cluster))
      ) |>
      dplyr::group_by(.cluster) |>
      dplyr::summarize(
        wss = sum(dist),
        n_obs = dplyr::n()
      )
  }

  return(res)
}

#' Compute the sum of within-cluster SSE
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids. Defaults
#'   to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @details Not to be confused with [sse_within()] that returns a tibble
#'   with within-cluster SSE, one row for each cluster.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @family cluster metric
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' sse_within_total(kmeans_fit)
#'
#' sse_within_total_vec(kmeans_fit)
#' @export
sse_within_total <- function(object, ...) {
  UseMethod("sse_within_total")
}

sse_within_total <- new_cluster_metric(
  sse_within_total,
  direction = "zero"
)

#' @export
#' @rdname sse_within_total
sse_within_total.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname sse_within_total
sse_within_total.cluster_fit <- function(
  object,
  new_data = NULL,
  dist_fun = NULL,
  ...
) {
  if (is.null(dist_fun)) {
    dist_fun <- function(x, y) {
      philentropy::dist_many_many(x, y, method = "euclidean")
    }
  }

  res <- sse_within_total_impl(object, new_data, dist_fun, ...)

  tibble::tibble(
    .metric = "sse_within_total",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname sse_within_total
sse_within_total.workflow <- sse_within_total.cluster_fit

#' @export
#' @rdname sse_within_total
sse_within_total_vec <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  sse_within_total_impl(object, new_data, dist_fun, ...)
}

sse_within_total_impl <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  sum(sse_within(object, new_data, dist_fun, ...)$wss, na.rm = TRUE)
}

#' Compute the total sum of squares
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#'   to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @family cluster metric
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' sse_total(kmeans_fit)
#'
#' sse_total_vec(kmeans_fit)
#' @export
sse_total <- function(object, ...) {
  UseMethod("sse_total")
}

sse_total <- new_cluster_metric(
  sse_total,
  direction = "zero"
)

#' @export
#' @rdname sse_total
sse_total.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname sse_total
sse_total.cluster_fit <- function(
  object,
  new_data = NULL,
  dist_fun = NULL,
  ...
) {
  if (is.null(dist_fun)) {
    dist_fun <- function(x, y) {
      philentropy::dist_many_many(x, y, method = "euclidean")
    }
  }

  res <- sse_total_impl(object, new_data, dist_fun, ...)

  tibble::tibble(
    .metric = "sse_total",
    .estimator = "standard",
    .estimate = res
  )
}

#' @export
#' @rdname sse_total
sse_total.workflow <- sse_total.cluster_fit

#' @export
#' @rdname sse_total
sse_total_vec <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  sse_total_impl(object, new_data, dist_fun, ...)
}

sse_total_impl <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {
    new_data <- extract_post_preprocessor(object, new_data)
  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {
    tot <- summ$sse_total
  } else {
    overall_mean <- colSums(summ$centroids * summ$n_members) /
      sum(summ$n_members)
    suppressMessages(
      tot <- dist_fun(t(as.matrix(overall_mean)), as.matrix(new_data))^2 |>
        sum()
    )
  }

  return(tot)
}

#' Compute the ratio of the WSS to the total SSE
#'
#' @param object A fitted kmeans tidyclust model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#'   to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @family cluster metric
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' sse_ratio(kmeans_fit)
#'
#' sse_ratio_vec(kmeans_fit)
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
sse_ratio.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname sse_ratio
sse_ratio.cluster_fit <- function(
  object,
  new_data = NULL,
  dist_fun = NULL,
  ...
) {
  if (is.null(dist_fun)) {
    dist_fun <- function(x, y) {
      philentropy::dist_many_many(x, y, method = "euclidean")
    }
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
sse_ratio_vec <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  sse_ratio_impl(object, new_data, dist_fun, ...)
}

sse_ratio_impl <- function(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  },
  ...
) {
  sse_within_total_vec(object, new_data, dist_fun) /
    sse_total_vec(object, new_data, dist_fun)
}
