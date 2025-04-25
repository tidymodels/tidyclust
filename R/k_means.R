#' K-Means
#'
#' @description
#'
#' `k_means()` defines a model that fits clusters based on distances to a number
#' of centers. This definition doesn't just include K-means, but includes
#' models like K-prototypes.
#'
#' There are different ways to fit this model, and the method of estimation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_k_means_stats]{stats}: Classical K-means
#' - \link[=details_k_means_ClusterR]{ClusterR}: Classical K-means
#' - \link[=details_k_means_klaR]{klaR}: K-Modes
#' - \link[=details_k_means_clustMixType]{clustMixType}: K-prototypes
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"stats"`.
#' @param num_clusters Positive integer, number of clusters in model.
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' For a K-means model, each cluster is defined by a location in the predictor
#' space. Therefore, prediction in tidyclust is defined by calculating which
#' cluster centroid an observation is closest too.
#'
#' @return A `k_means` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("k_means")
#'
#' k_means()
#' @export
k_means <-
  function(mode = "partition", engine = "stats", num_clusters = NULL) {
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
update.k_means <- function(
  object,
  parameters = NULL,
  num_clusters = NULL,
  fresh = FALSE,
  ...
) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh,
    ...
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

#' @export
check_args.k_means <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters < 0)) {
    cli::cli_abort("The number of centers should be >= 0.")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around ClusterR kmeans
#'
#' This wrapper runs `ClusterR::KMeans_rcpp()` and adds column names to the
#' `centroids` field. And reorders the clusters.
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
.k_means_fit_ClusterR <- function(
  data,
  clusters,
  num_init = 1,
  max_iters = 100,
  initializer = "kmeans++",
  fuzzy = FALSE,
  verbose = FALSE,
  CENTROIDS = NULL,
  tol = 1e-04,
  tol_optimal_init = 0.3,
  seed = 1
) {
  if (is.null(clusters)) {
    cli::cli_abort(
      "Please specify {.arg num_clust} to be able to fit specification.",
      call = call("fit")
    )
  }

  res <- ClusterR::KMeans_rcpp(
    data,
    clusters,
    num_init = num_init,
    max_iters = max_iters,
    initializer = initializer,
    fuzzy = fuzzy,
    verbose = verbose,
    CENTROIDS = CENTROIDS,
    tol = tol,
    tol_optimal_init = tol_optimal_init,
    seed = seed
  )

  colnames(res$centroids) <- colnames(data)

  new_order <- unique(res$clusters)
  res$clusters <- order(new_order)[res$clusters]
  res$centroids <- res$centroids[new_order, , drop = FALSE]
  res$WCSS_per_cluster <- res$WCSS_per_cluster[, new_order, drop = FALSE]
  res$obs_per_cluster <- res$obs_per_cluster[, new_order, drop = FALSE]
  res
}

#' Simple Wrapper around stats kmeans
#'
#' This wrapper runs `stats::kmeans()` and adds a check that `centers` is
#' specified. And reorders the clusters.
#'
#' @inheritParams stats::kmeans
#' @param ... Other arguments passed to `stats::kmeans()`
#'
#' @return Result from `stats::kmeans()`
#' @keywords internal
#' @export
.k_means_fit_stats <- function(data, centers = NULL, ...) {
  if (is.null(centers)) {
    cli::cli_abort(
      "Please specify {.arg num_clust} to be able to fit specification.",
      call = call("fit")
    )
  }

  res <- stats::kmeans(data, centers, ...)
  new_order <- unique(res$cluster)
  res$cluster <- set_names(order(new_order)[res$cluster], names(res$cluster))
  res$centers <- res$centers[new_order, , drop = FALSE]
  res$withinss <- res$withinss[new_order]
  res$size <- res$size[new_order]
  res
}

#' Simple Wrapper around clustMixType kmeans
#'
#' This wrapper runs `clustMixType::kproto()` and reorders the clusters.
#'
#' @inheritParams clustMixType::kproto
#' @param ... Other arguments passed to `clustMixType::kproto()`
#'
#' @return Result from `clustMixType::kproto()`
#' @keywords internal
#' @export
.k_means_fit_clustMixType <- function(x, k, ...) {
  res <- tryCatch(
    clustMixType::kproto(x, k, ...),
    error = function(cnd) {
      if (grepl("No factor variables", cnd$message)) {
        cli::cli_abort(
          c(
            "Engine `clustMixType` requires both numeric and categorical \\
          predictors.",
            "x" = "Only numeric predictors where used.",
            "i" = "Try using the `stats` engine with \\
          {.code mod |> set_engine(\"stats\")}."
          ),
          call = call("fit")
        )
      }
      if (grepl("No numeric variables", cnd$message)) {
        cli::cli_abort(
          c(
            "Engine `clustMixType` requires both numeric and categorical \\
          predictors.",
            "x" = "Only categorical predictors where used.",
            "i" = "Try using the `klaR` engine with \\
          {.code mod |> set_engine(\"klaR\")}."
          ),
          call = call("fit")
        )
      }
      stop(cnd)
    }
  )

  new_order <- unique(res$cluster)
  res$cluster <- order(new_order)[res$cluster]
  res$centers <- res$centers[new_order, , drop = FALSE]
  res$withinss <- res$withinss[new_order]
  res$dists <- res$dists[, new_order, drop = FALSE]
  res$size <- res$size[new_order]
  res
}

#' Simple Wrapper around klaR kmeans
#'
#' This wrapper runs `klaR::kmodes()` and reorders the clusters.
#'
#' @inheritParams klaR::kmodes
#' @param ... Other arguments passed to `klaR::kmodes()`
#'
#' @return Result from `klaR::kmodes()`
#' @keywords internal
#' @export
.k_means_fit_klaR <- function(data, modes, ...) {
  res <- klaR::kmodes(data, modes, ...)
  new_order <- unique(res$cluster)
  res$cluster <- order(new_order)[res$cluster]
  res$size <- res$size[new_order]
  res$modes <- res$modes[new_order, , drop = FALSE]
  res$withindiff <- res$withindiff[new_order]
  res
}
