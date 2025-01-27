#' Hierarchical (Agglomerative) Clustering
#'
#' @description
#'
#' `hier_clust()` defines a model that fits clusters based on a distance-based
#' dendrogram
#'
#' There are different ways to fit this model, and the method of estimation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_hier_clust_stats]{stats}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"stats"`.
#' @param num_clusters Positive integer, number of clusters in model (optional).
#' @param cut_height Positive double, height at which to cut dendrogram to
#'   obtain cluster assignments (only used if `num_clusters` is `NULL`)
#' @param linkage_method the agglomeration method to be used. This should be (an
#'   unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#'   `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"` (=
#'   WPGMC) or `"centroid"` (= UPGMC).
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, we find the closest
#' cluster. How we measure “closeness” is dependent on the specified type of
#' linkage in the model:
#'
#' - *single linkage*: The new observation is assigned to the same cluster as
#'   its nearest observation from the training data.
#' - *complete linkage*: The new observation is assigned to the cluster with the
#'   smallest maximum distances between training observations and the new
#'   observation.
#' - *average linkage*: The new observation is assigned to the cluster with the
#'   smallest average distances between training observations and the new
#'   observation.
#' - *centroid method*: The new observation is assigned to the cluster with the
#'   closest centroid, as in prediction for k_means.
#' - *Ward’s method*: The new observation is assigned to the cluster with the
#'   smallest increase in **error sum of squares (ESS)** due to the new
#'   addition. The ESS is computed as the sum of squared distances between
#'   observations in a cluster, and the centroid of the cluster.
#'
#' @return A `hier_clust` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("hier_clust")
#'
#' hier_clust()
#' @export
hier_clust <-
  function(
    mode = "partition",
    engine = "stats",
    num_clusters = NULL,
    cut_height = NULL,
    linkage_method = "complete"
  ) {
    args <- list(
      num_clusters = enquo(num_clusters),
      cut_height = enquo(cut_height),
      linkage_method = enquo(linkage_method)
    )

    new_cluster_spec(
      "hier_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.hier_clust <- function(x, ...) {
  cat("Hierarchical Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update hier_clust
#' @rdname tidyclust_update
#' @export
update.hier_clust <- function(
  object,
  parameters = NULL,
  num_clusters = NULL,
  cut_height = NULL,
  linkage_method = NULL,
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
    num_clusters = enquo(num_clusters),
    cut_height = enquo(cut_height),
    linkage_method = enquo(linkage_method)
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
    "hier_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.hier_clust <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters < 0)) {
    cli::cli_abort("The number of centers should be >= 0.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.hier_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around hclust function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `stats::hclust` and retains the parameters `num_clusters` or `h` as an
#' attribute.
#'
#' @param x matrix or data frame
#' @param num_clusters the number of clusters
#' @param cut_height the height to cut the dendrogram
#' @param linkage_method the agglomeration method to be used. This should be (an
#'   unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#'   `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"` (=
#'   WPGMC) or `"centroid"` (= UPGMC).
#' @param dist_fun A distance function to use
#'
#' @return A dendrogram
#' @keywords internal
#' @export
.hier_clust_fit_stats <- function(
  x,
  num_clusters = NULL,
  cut_height = NULL,
  linkage_method = NULL,
  dist_fun = philentropy::distance
) {
  suppressMessages(
    dmat <- dist_fun(x)
  )
  res <- stats::hclust(stats::as.dist(dmat), method = linkage_method)
  attr(res, "num_clusters") <- num_clusters
  attr(res, "cut_height") <- cut_height
  attr(res, "training_data") <- x
  return(res)
}
