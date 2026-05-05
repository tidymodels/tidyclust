#' Mean Shift Clustering
#'
#' @description
#'
#' `mean_shift()` defines a model that fits clusters by iteratively shifting
#' observations toward regions of high density, with the number of clusters
#' determined automatically from the data.
#'
#' There are different implementations for this model, and the implementation
#' is chosen by setting the model engine. The engine-specific pages for this
#' model are listed below.
#'
#' - \link[=details_mean_shift_LPCM]{LPCM}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is `"partition"`.
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The default engine for this model is `"LPCM"`.
#' @param bandwidth Positive double, kernel bandwidth controlling the size of
#'   the neighborhood used to compute the density estimate (required).
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, the mean shift
#' procedure is run from the new point until it converges to a mode. The
#' observation is then assigned to the cluster of the nearest discovered
#' training mode.
#'
#' @return A `mean_shift` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("mean_shift")
#'
#' mean_shift()
#' @export
mean_shift <-
  function(
    mode = "partition",
    engine = "LPCM",
    bandwidth = NULL
  ) {
    args <- list(
      bandwidth = enquo(bandwidth)
    )

    new_cluster_spec(
      "mean_shift",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.mean_shift <- function(x, ...) {
  cat("Mean Shift Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update mean_shift
#' @rdname tidyclust_update
#' @export
update.mean_shift <- function(
  object,
  parameters = NULL,
  bandwidth = NULL,
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
    bandwidth = enquo(bandwidth)
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
    "mean_shift",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# ------------------------------------------------------------------------------

#' @export
check_args.mean_shift <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$bandwidth)) && any(args$bandwidth <= 0)) {
    cli::cli_abort("The bandwidth used for clustering should be > 0.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.mean_shift <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around LPCM::ms function
#'
#' This wrapper passes the data and bandwidth to `LPCM::ms()` with plotting
#' disabled.
#'
#' @param x matrix or data frame.
#' @param bandwidth Kernel bandwidth controlling the neighborhood size.
#'
#' @return ms object
#' @keywords internal
#' @export
.mean_shift_fit_LPCM <- function(x, bandwidth = NULL, ...) {
  if (is.null(bandwidth)) {
    cli::cli_abort(
      "Please specify `bandwidth` to be able to fit specification.",
      call = call("fit")
    )
  }

  LPCM::ms(X = x, h = bandwidth, plot = FALSE, ...)
}
