#' Estimate the Average Jaccard Index for Cluster Stability
#'
#' This function estimates the stability of cluster assignments by calculating
#' the average Jaccard index: how well do cluster assignments of new data from a
#' pre-trained model align with cluster assignments of the same data from a new
#' model fitted on the same data (Hennig, 2007).
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
#' @references Hennig, C. (2007). Cluster-wise assessment of cluster stability.
#'   _Computational Statistics & Data Analysis, 52_(1), 258-271.
#'
#' @family cluster metric
#'
#' @seealso [fpc::clusterboot()]
#'
#' @examples
#' library(tidymodels)
#'
#' penguins <- drop_na(penguins)
#'
#' kmeans_spec <- k_means(num_clusters = tune())
#'
#' res <- tune_cluster(
#'   kmeans_spec,
#'   ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'   resamples = bootstraps(penguins, times = 10),
#'   metrics = cluster_metric_set(jaccard_avg)
#' )
#'
#' # What's the largest k that has high recovery rate?
#' # (values above 0.75 are considered good, values below 0.5 are considered bad)
#' autoplot(res)
#'
#' @export
jaccard_avg <- function(object, ...) {
  UseMethod("jaccard_avg")
}

jaccard_avg <- new_cluster_metric(
  jaccard_avg,
  direction = "maximize"
)

#' @export
#' @rdname jaccard_avg
jaccard_avg.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname jaccard_avg
jaccard_avg.cluster_fit <- function(object, new_data = NULL, ...) {
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
  jaccard_avg_impl(pred_fit, pred_new_fit)
}

#' @export
#' @rdname jaccard_avg
jaccard_avg.workflow <- function(object, new_data = NULL, ...) {
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
  jaccard_avg_impl(pred_fit, pred_new_fit)
}

# Core vector helper for Jaccard index calculation
jaccard_avg_impl <- function(pred_clusters, pred_clusters_new) {
  # 1. Create a contingency table (Rows = test clusters, Cols = train clusters)
  # tab[i, j] gives the size of the intersection between test cluster i and train cluster j
  tab <- as.matrix(table(pred_clusters, pred_clusters_new))

  # 2. Extract cluster sizes
  row_sums <- rowSums(tab) # Sizes of reference (test) clusters
  col_sums <- colSums(tab) # Sizes of comparison (train) clusters

  # 3. Compute the Union matrix: |A| + |B| - |A,B|
  # outer() creates a matrix of all combinations of (|A| + |B|)
  union_matrix <- outer(row_sums, col_sums, "+") - tab

  # 4. Compute the Jaccard matrix: Intersection / Union
  jaccard_avg_matrix <- tab / union_matrix

  # 5. For each reference (test) cluster, find its maximum Jaccard alignment
  cluster_stabilities <- apply(jaccard_avg_matrix, 1, max)

  tibble::tibble(
    .metric = "jaccard",
    .estimator = "standard",
    .estimate = mean(cluster_stabilities)
  )
}
