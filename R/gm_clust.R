#' Gaussian Mixture Models (GMM)
#'
#' @description
#'
#' `gm_clust` defines a model that fits clusters based on fitting a specified number of
#' multivariate Gaussian distributions (MVG) to the data.
#'
#' There are multiple implementations for this model, and the implementation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_gm_clust_mclust]{mclust}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The engine for this model is `"mclust"`.
#' @param num_clusters Positive integer, number of clusters in model (required).
#' @param circular Boolean, whether or not to fit circular MVG distributions for each cluster. Default `TRUE`.
#' @param zero_covariance Boolean, whether or not to assign covariances of 0 for each MVG. Default `TRUE`.
#' @param shared_orientation Boolean, whether each cluster MVG should have the same orientation. Default `TRUE`.
#' @param shared_shape Boolean, whether each cluster MVG should have the same shape. Default `TRUE`.
#' @param shared_size Boolean, whether each cluster MVG should have the same size/volume. Default `TRUE`.
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, we determine which cluster
#' a point has the highest probability of belonging to.
#'
#'
#' @return A `gm_clust` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("gm_clust")
#'
#' gm_clust()
#' @export
gm_clust <-
  function(mode = "partition",
           engine = "mclust",
           num_clusters = NULL,
           circular = TRUE,
           shared_size = TRUE,
           zero_covariance = TRUE,
           shared_orientation = TRUE,
           shared_shape = TRUE
           ) {
    args <- list(
      num_clusters = enquo(num_clusters),
      circular = enquo(circular),
      zero_covariance = enquo(zero_covariance),
      shared_orientation = enquo(shared_orientation),
      shared_shape = enquo(shared_shape),
      shared_size = enquo(shared_size)
    )

    new_cluster_spec(
      "gm_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.gm_clust <- function(x, ...) {
  cat("GMM Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update gm_clust
#' @rdname tidyclust_update
#' @export
update.gm_clust <- function(object,
                            parameters = NULL,
                            num_clusters = NULL,
                            circular = NULL,
                            zero_covariance = NULL,
                            shared_orientation = NULL,
                            shared_shape = NULL,
                            shared_size = NULL,
                            fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    num_clusters = enquo(num_clusters),
    circular = enquo(circular),
    zero_covariance = enquo(zero_covariance),
    shared_orientation = enquo(shared_orientation),
    shared_shape = enquo(shared_shape),
    shared_size = enquo(shared_size)
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
    "gm_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.gm_clust <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters <= 0)) {
    cli::cli_abort("The number of clusters should be > 0.")
  }

  if (length(args$num_clusters) > 1) {
    cli::cli_abort("The number of clusters should be a single number.")
  }

  if (all(!is.logical(args$circular))) {
    cli::cli_abort("The circular cluster shape argument should be TRUE or FALSE.")
  }

  if (all(!is.logical(args$zero_covariance))) {
    cli::cli_abort("The zero covariance argument should be TRUE or FALSE.")
  }

  if (all(!is.logical(args$shared_orientation))) {
    cli::cli_abort("The shared cluster orientation argument should be TRUE or FALSE.")
  }

  if (all(!is.logical(args$shared_shape))) {
    cli::cli_abort("The shared cluster shape argument should be TRUE or FALSE.")
  }

  if (all(!is.logical(args$shared_size))) {
    cli::cli_abort("The shared cluster size argument should be TRUE or FALSE.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.gm_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around Mclust function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `mclust::Mclust` and retains the parameters `num_clusters` as an
#' attribute.
#'
#' @param x matrix or data frame.
#' @param num_clusters Number of clusters.
#' @param circular Whether or not to fit circular MVG distributions for each cluster.
#' @param zero_covariance Whether or not to assign covariances of 0 for each MVG.
#' @param shared_orientation Whether each cluster MVG should have the same orientation.
#' @param shared_shape Whether each cluster MVG should have the same shape.
#' @param shared_size Whether each cluster MVG should have the same size/volume.
#'
#' @return mclust object
#' @keywords internal
#' @export
.gm_clust_fit_mclust <- function(x,
                                 num_clusters = NULL,
                                 circular = NULL,
                                 zero_covariance = NULL,
                                 shared_orientation = NULL,
                                 shared_shape = NULL,
                                 shared_size = NULL,
                                 ...) {
  if (is.null(num_clusters)) {
    cli::cli_abort(
      "Please specify `num_clusters` to be able to fit specification.",
      call = call("fit")
    )
  }
  if (is.null(circular)) {
    cli::cli_abort(
      "Please specify `circular` to be able to fit specification.",
      call = call("fit")
    )
  }
  if (is.null(shared_size)) {
    cli::cli_abort(
      "Please specify `shared_size` to be able to fit specification.",
      call = call("fit")
    )
  }
  if (!circular) {

    if (is.null(zero_covariance)) {
      cli::cli_abort(
        "Please specify `zero_covariance` to be able to fit specification.",
        call = call("fit")
      )
    }

    if (is.null(shared_shape)) {
      cli::cli_abort(
        "Please specify `shared_shape` to be able to fit specification.",
        call = call("fit")
      )
    }

    if (!zero_covariance) {

      if (is.null(shared_orientation)) {
        cli::cli_abort(
          "Please specify `shared_orientation` to be able to fit specification.",
          call = call("fit")
        )
      }
    }
  }

  if (circular) {
    if (!zero_covariance) {
      warning("circular = TRUE so zero_covariance = FALSE has no effect on the fitted model specification")
    }
    if (!shared_orientation) {
      warning("circular = TRUE so shared_orientation = FALSE has no effect on the fitted model specification")
    }
    if (!shared_shape) {
      warning("circular = TRUE so shared_shape = FALSE has no effect on the fitted model specification")
    }

  } else {
    if (zero_covariance) {
      if (!shared_orientation) {
        warning("zero_covariance = TRUE so shared_orientation = FALSE has no effect on the fitted model specification")
      }
    }
  }



  model_name <- mclust_helper(circular,
                              zero_covariance,
                              shared_orientation,
                              shared_shape,
                              shared_size)



  res <- mclust::Mclust(x, G = num_clusters, modelNames = model_name)

  if (is.null(res)) {
    cli::cli_abort(
      "Model cannot be estimated. Please specify a model specification with less parameters by setting some model arguments to TRUE",
      call = call("fit")
    )
  }

  attr(res, "num_clusters") <- num_clusters
  attr(res, "circular") <- circular
  attr(res, "zero_covariance") <- zero_covariance
  attr(res, "shared_orientation") <- shared_orientation
  attr(res, "shared_shape") <- shared_shape
  attr(res, "shared_size") <- shared_size
  attr(res, "model_name") <- model_name
  attr(res, "training_data") <- x
  res
}


#' mclust fit helper function
#'
#' This function returns the mclust model name based on the specified
#' TRUE/FALSE model arguments.
#'
#' @param circular Whether or not to fit circular MVG distributions for each cluster.
#' @param zero_covariance Whether or not to assign covariances of 0 for each MVG.
#' @param shared_orientation Whether each cluster MVG should have the same orientation.
#' @param shared_shape Whether each cluster MVG should have the same shape.
#' @param shared_size Whether each cluster MVG should have the same size/volume.
#'
#' @return string containing mclust model name
#' @keywords internal
mclust_helper <- function(circular,
                          zero_covariance,
                          shared_orientation,
                          shared_shape,
                          shared_size) {
  model_name <- dplyr::case_when(
    circular & shared_size ~ "EII",
    circular & !shared_size ~ "VII",
    !circular & zero_covariance  & shared_shape & shared_size ~ "EEI",
    !circular & zero_covariance  & !shared_shape & shared_size ~ "EVI",
    !circular & zero_covariance  & shared_shape & !shared_size ~ "VEI",
    !circular & zero_covariance  & !shared_shape & !shared_size ~ "VVI",
    !circular & !zero_covariance & shared_orientation & shared_shape & shared_size ~ "EEE",
    !circular & !zero_covariance & shared_orientation & !shared_shape & shared_size ~ "EVE",
    !circular & !zero_covariance & shared_orientation & shared_shape & !shared_size ~ "VEE",
    !circular & !zero_covariance & shared_orientation & !shared_shape & !shared_size ~ "VVE",
    !circular & !zero_covariance & !shared_orientation & shared_shape & shared_size ~ "EEV",
    !circular & !zero_covariance & !shared_orientation & !shared_shape & shared_size ~ "EVV",
    !circular & !zero_covariance & !shared_orientation & shared_shape & !shared_size ~ "VEV",
    !circular & !zero_covariance & !shared_orientation & !shared_shape & !shared_size ~ "VVV"
  )
  model_name
}

