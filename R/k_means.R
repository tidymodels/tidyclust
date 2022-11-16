#' K-Means
#'
#' @description
#'
#' `k_means()` defines a model that fits clusters based on distances to a number
#' of centers.
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"stats"`.
#' @param num_clusters Positive integer, number of clusters in model.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("k_means")
#'
#' k_means()
#' @export
k_means <-
  function(mode = "partition",
           engine = "stats",
           num_clusters = NULL) {
    args <- list(
      num_clusters = enquo(num_clusters)
    )

    new_cluster_spec(
      "k_means",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.k_means <- function(x, ...) {
  cat("K Means Cluster Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

#' @export
translate_tidyclust.k_means <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' @method update k_means
#' @rdname tidyclust_update
#' @export
update.k_means <- function(object,
                           parameters = NULL,
                           num_clusters = NULL,
                           fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args, fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    num_clusters = enquo(num_clusters)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  new_cluster_spec(
    "k_means",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# ------------------------------------------------------------------------------

check_args.k_means <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters < 0)) {
    rlang::abort("The number of centers should be >= 0.")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around ClusterR kmeans
#'
#' This wrapper runs `ClusterR::KMeans_rcpp` and adds column names to the
#' `centroids` field.
#'
#' @param data matrix or data frame
#' @param clusters the number of clusters
#' @param num_init number of times the algorithm will be run with different
#'   centroid seeds
#' @param max_iters the maximum number of clustering iterations
#' @param initializer the method of initialization. One of, optimal_init,
#'   quantile_init, kmeans++ and random. See details for more information
#' @param fuzzy either TRUE or FALSE. If TRUE, then prediction probabilities
#'   will be calculated using the distance between observations and centroids
#' @param verbose either TRUE or FALSE, indicating whether progress is printed
#'   during clustering.
#' @param CENTROIDS a matrix of initial cluster centroids. The rows of the
#'   CENTROIDS matrix should be equal to the number of clusters and the columns
#'   should be equal to the columns of the data.
#' @param tol a float number. If, in case of an iteration (iteration > 1 and
#'   iteration < max_iters) 'tol' is greater than the squared norm of the
#'   centroids, then kmeans has converged
#' @param tol_optimal_init tolerance value for the 'optimal_init' initializer.
#'   The higher this value is, the far appart from each other the centroids are.
#' @param seed integer value for random number generator (RNG)
#'
#' @return a list with the following attributes: clusters, fuzzy_clusters (if
#'   fuzzy = TRUE), centroids, total_SSE, best_initialization, WCSS_per_cluster,
#'   obs_per_cluster, between.SS_DIV_total.SS
#' @keywords internal
#' @export
ClusterR_kmeans_fit <- function(data, clusters, num_init = 1, max_iters = 100,
                                initializer = "kmeans++", fuzzy = FALSE,
                                verbose = FALSE, CENTROIDS = NULL, tol = 1e-04,
                                tol_optimal_init = 0.3, seed = 1) {
  res <- ClusterR::KMeans_rcpp(data, clusters)
  colnames(res$centroids) <- colnames(data)
  res
}
