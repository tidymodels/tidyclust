# https://github.com/tidymodels/parsnip/blob/main/R/augment.R

#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' For partition models, a `.pred_cluster` column is added.
#'
#' @param x A [`cluster_fit`] object produced by [fit.cluster_spec()] or
#'   [fit_xy.cluster_spec()].
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
#' @rdname augment
#' @return A tibble containing `new_data` with a `.pred_cluster` column
#'   appended giving the cluster assignment for each row.
#'
#' @details
#' ## Preprocessing with workflows
#'
#' When `x` is a fitted [workflows::workflow()] that includes a recipe, the
#' recipe transformations are applied to `new_data` before predicting. The
#' returned tibble contains the **original** (untransformed) `new_data` plus
#' the `.pred_cluster` column, so the data is not altered by preprocessing.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit |>
#'   augment(new_data = mtcars)
#'
#' # With a workflow that includes a recipe
#' library(recipes)
#' library(workflows)
#'
#' rec <- recipe(~., data = mtcars) |>
#'   step_normalize(all_predictors())
#'
#' wf_fit <- workflow() |>
#'   add_recipe(rec) |>
#'   add_model(kmeans_spec) |>
#'   fit(data = mtcars)
#'
#' # Returns original (untransformed) data with .pred_cluster appended
#' augment(wf_fit, new_data = mtcars)
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
