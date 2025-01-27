#' Prepares data and distance matrices for metric calculation
#'
#' @param object A fitted [`cluster_spec`] object.
#' @param new_data A dataset to calculate predictions on.  If `NULL`, the
#'   trained cluster assignments from the fitted object are used.
#' @param dists A distance matrix for the data.  If `NULL`, distance is computed
#'   on `new_data` using the `stats::dist()` function.
#' @param dist_fun A custom distance functions.
#'
#' @return A list
prep_data_dist <- function(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance
) {
  # Sihouettes requires a distance matrix
  if (is.null(new_data) && is.null(dists)) {
    cli::cli_abort(
      "Must supply either a dataset or distance matrix to compute silhouettes."
    )
  }

  # If data is blank, we are using the trained cluster assignments
  if (is.null(new_data)) {
    clusters <- extract_fit_summary(object)$cluster_assignments
  } else {
    clusters <- predict(object, new_data)$.pred_cluster
  }

  # If they supplied distance, check that it matches the data dimension
  if (!is.null(dists)) {
    if (!is.null(new_data) && nrow(new_data) != attr(dists, "Size")) {
      cli::cli_abort("Dimensions of dataset and distance matrix must match.")
    } else if (is.null(new_data) && length(clusters) != attr(dists, "Size")) {
      cli::cli_abort(
        "Dimensions of training dataset and distance matrix must match."
      )
    }
  }

  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {
    new_data <- extract_post_preprocessor(object, new_data)
  }

  # Calculate distances including optionally supplied params
  if (is.null(dists)) {
    suppressMessages(
      dists <- dist_fun(new_data)
    )
  }

  return(
    list(
      clusters = clusters,
      data = new_data,
      dists = dists
    )
  )
}

#' Computes distance from observations to centroids
#'
#' @param new_data A data frame
#' @param centroids A data frame where each row is a centroid.
#' @param dist_fun A function for computing matrix-to-matrix distances. Defaults
#'   to
#'   `function(x, y) philentropy::dist_many_many(x, y, method = "euclidean")`.
get_centroid_dists <- function(
  new_data,
  centroids,
  dist_fun = function(x, y) {
    philentropy::dist_many_many(x, y, method = "euclidean")
  }
) {
  if (ncol(new_data) != ncol(centroids)) {
  }

  suppressMessages(
    dist_fun(as.matrix(centroids), as.matrix(new_data))
  )
}
