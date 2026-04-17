#' Density-Based Spatial Clustering of Applications with Noise (DBSCAN)
#'
#' @description
#'
#' `db_clust` defines a model that fits clusters based on areas with observations
#' that are densely packed together using the DBSCAN algorithm
#'
#' There are multiple implementations for this model, and the implementation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_db_clust_dbscan]{dbscan}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is `"partition"`.
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The engine for this model is `"dbscan"`.
#' @param radius Positive double, Radius drawn around points to determine core-points and cluster assignments (required).
#' @param min_points Positive integer, Minimum number of connected points required to form a core-point, including the point itself (required).
#'
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, we determine if a point
#' is within the radius of a core point. If so, we predict the same cluster as the core point.
#' If not, we predict the observation to be an outlier.
#'
#'
#' @return A `db_clust` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("db_clust")
#'
#' db_clust()
#' @export
db_clust <-
  function(mode = "partition",
           engine = "dbscan",
           radius = NULL,
           min_points = NULL) {
    args <- list(
      radius = enquo(radius),
      min_points = enquo(min_points)
    )

    new_cluster_spec(
      "db_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.db_clust <- function(x, ...) {
  cat("DBSCAN Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update db_clust
#' @rdname tidyclust_update
#' @export
update.db_clust <- function(object,
                              parameters = NULL,
                              radius = NULL,
                              min_points = NULL,
                              fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    radius = enquo(radius),
    min_points = enquo(min_points)
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
    "db_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.db_clust <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$min_points)) && any(args$min_points < 0)) {
    cli::cli_abort("The number of points in a cluster should be > 0.")
  }

  if (all(is.numeric(args$radius)) && any(args$radius < 0)) {
    cli::cli_abort("The radius used to create a cluster should be > 0.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.db_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around dbscan function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `dbscan::dbscan()` and retains the parameters `radius` or `min_points` as an
#' attribute.
#'
#' @param x matrix or data frame.
#' @param radius Radius used to determine core-points and cluster points together.
#' @param min_points Minimum number of points needed to form a cluster.
#'
#' @return dbscan object
#' @keywords internal
#' @export
.db_clust_fit_dbscan <- function(x,
                                 radius = NULL,
                                 min_points = NULL,
                                 ...) {
  if (is.null(radius)) {
    cli::cli_abort(
      "Please specify `radius` to be able to fit specification.",
      call = call("fit")
    )
  }

  if (is.null(min_points)) {
    cli::cli_abort(
      "Please specify `min_points` to be able to fit specification.",
      call = call("fit")
    )
  }

  res <- dbscan::dbscan(x, eps = radius, minPts = min_points)
  attr(res, "radius") <- radius
  attr(res, "min_points") <- min_points
  attr(res, "training_data") <- x
  is_core <- dbscan::is.corepoint(x, eps = radius, minPts = min_points)
  attr(res, "is_core") <- is_core

  res
}

#' dbscan fit helper function
#'
#' This function returns the cluster assignments for the training data
#' based on their distance to the CLOSEST core point in the data.
#'
#' @param object db_clust object
#'
#' @return numeric vector
#' @keywords internal
dbscan_helper <- function(object,
                          ...) {

  is_core <- attr(object, "is_core")
  training_data <- data.frame(attr(object, "training_data"))
  cp <- training_data[is_core,]
  non_cp <- training_data[!is_core,]
  cp_clusters <- object$cluster[is_core]
  eps <- attr(object, "radius")

  # if all points are core points then no border points / outliers to fit
  if (sum(is_core) == nrow(training_data)) {
    return(cp_clusters)
  }

  # if there are no core points, all points are considered outliers
  if (sum(is_core) == 0) {
    return(rep(0, nrow(training_data)))
  }

  # get fit values according to closest core point
  nn <- dbscan::frNN(cp,
             query = non_cp,
             eps = eps,
             sort = TRUE)

  non_cp_clusters <- vapply(
    nn$id, function(nns) if (length(nns) == 0L) 0L else cp_clusters[nns[1L]], integer(1L)
  )


  # join back separated fits into proper order in training data
  non_cp_clusters <- data.frame(non_cp_clusters)
  cp_clusters <- data.frame(cp_clusters)

  # create vars to join back results in proper order
  training_data$overall_order <- 1:nrow(training_data)
  training_data$is_core <- ifelse(is_core, "cp", "non cp")
  non_cp_clusters$is_core <- "non cp"
  cp_clusters$is_core <- "cp"

  training_data$id <- stats::ave(training_data$is_core, training_data$is_core, FUN = seq_along)
  non_cp_clusters$id <- 1:nrow(non_cp_clusters)
  cp_clusters$id <- 1:nrow(cp_clusters)

  training_data <- merge(x = training_data, y = non_cp_clusters, by = c("id", "is_core"), all.x = TRUE)
  training_data <- merge(x = training_data, y = cp_clusters, by = c("id", "is_core"), all.x = TRUE)

  training_data$cluster <- ifelse(!is.na(training_data$non_cp_clusters), training_data$non_cp_clusters, training_data$cp_clusters)
  training_data$cluster[order(training_data$overall_order)]

}
