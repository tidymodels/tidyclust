#' Estimate the Prediction Strength of a Clustering Model
#'
#' This function estimates the stability of cluster assignments by calculating
#' the prediction strength: how well do cluster assignments of new data from a
#' pre-trained model co-incide with cluster assignments of the same data from a
#' new model fitted on the same data (Tibshirani & Walther, 2005).
#'
#' @param object A fitted tidyclust model
#' @param new_data A dataset to predict on.
#' @param ... Not currently used.
#'
#' @details Note that this metric will re-fit the model on the new dataset
#' (`new_data`), and so might be computationally expensive.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @references Tibshirani, R. and Walther, G. (2005) Cluster Validation by
#'   Prediction Strength, _Journal of Computational and Graphical Statistics, 14_(3), 511-528.
#'
#' @family cluster metric
#'
#' @examples
#'
#' library(tidymodels)
#'
#' penguins <- drop_na(penguins)
#'
#' kmeans_spec <- k_means(num_clusters = tune())
#'
#' res <- tune_cluster(
#'   kmeans_spec,
#'   ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'   resamples = vfold_cv(penguins, v = 2, repeats = 10),
#'   metrics = cluster_metric_set(pred_strength)
#' )
#'
#' # What's the largest k that has a good prediction strength?
#' # (values above 0.8 are considered good)
#' autoplot(res)
#'
#' @export
pred_strength <- function(object, ...) {
  UseMethod("pred_strength")
}

pred_strength <- new_cluster_metric(
  pred_strength,
  direction = "maximize"
)

#' @export
#' @rdname pred_strength
pred_strength.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname pred_strength
pred_strength.cluster_fit <- function(object, new_data = NULL, ...) {
  spec <- object$spec

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- fit(
    spec,
    ~.,
    data = new_data[, attr(object$preproc$terms, "term.labels"), drop = FALSE]
  )

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  pred_strength_impl(pred_fit, pred_new_fit)
}

#' @export
#' @rdname pred_strength
pred_strength.workflow <- function(object, new_data = NULL, ...) {
  if (!workflows::is_trained_workflow(object)) {
    stop("The workflow must be fitted before calculating cluster metrics.")
  }

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- fit(object, data = new_data)

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  pred_strength_impl(pred_fit, pred_new_fit)
}


# Core vector helper for prediction strength calculation
pred_strength_impl <- function(pred_clusters, pred_clusters_new) {
  split_preds <- split(pred_clusters, pred_clusters_new)
  ps_by_cluster <- purrr::map_dbl(split_preds, function(preds) {
    n <- length(preds)
    if (n <= 1) {
      return(1.0)
    }
    counts <- as.vector(table(as.character(preds)))
    matching_pairs <- sum(counts * (counts - 1))
    matching_pairs / (n * (n - 1))
  })

  # Return structured tibble standard for tidyclust metrics
  tibble::tibble(
    .metric = "pred_strength",
    .estimator = "standard",
    .estimate = min(ps_by_cluster)
  )
}
