# https://github.com/tidymodels/parsnip/blob/main/R/augment.R

#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' For partition models, a `.pred_cluster` column is added.
#'
#' @param x A [`cluster_fit`] object produced by [fit.cluster_spec()] or
#'   [fit_xy.cluster_spec()] .
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
#' @rdname augment
#' @return A `tibble::tibble()` with containing `new_data` with columns added
#'   depending on the mode of the model.
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit |>
#'   augment(new_data = mtcars)
#' @export
augment.cluster_fit <- function(x, new_data, ...) {
  ret <- new_data
  if (x$spec$mode == "partition") {
    check_spec_pred_type(x, "cluster")
    ret <- dplyr::bind_cols(
      ret,
      stats::predict(x, new_data = new_data)
    )
  } else {
    cli::cli_abort("Unknown mode: {x$spec$mode}")
  }
  as_tibble(ret)
}
