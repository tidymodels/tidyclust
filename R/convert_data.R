# https://github.com/tidymodels/parsnip/blob/main/R/convert_data.R

#' Helper functions to convert between formula and matrix interface
#'
#' @description Functions to take a formula interface and get the resulting
#'   objects (y, x, weights, etc) back or the other way around. The functions
#'   are intended for developer use. For the most part, this emulates the
#'   internals of `lm()` (and also see the notes at
#'   https://developer.r-project.org/model-fitting-functions.html).
#'
#'   `.convert_form_to_x_fit()` is for when the data are created for modeling.
#'   It saves both the data objects as well as the objects needed when new data
#'   are predicted (e.g. `terms`, etc.).
#'
#'   `.convert_form_to_x_new()` is used when new samples are being predicted and
#'   only requires the predictors to be available.
#'
#' @param data A data frame containing all relevant variables (e.g. predictors,
#'   case weights, etc).
#' @param ... Additional arguments passed to [stats::model.frame()].
#' @param na.action A function which indicates what should happen when the data
#'   contain NAs.
#' @param indicators A string describing whether and how to create
#'   indicator/dummy variables from factor predictors. Possible options are
#'   `"none"`, `"traditional"`, and `"one_hot"`.
#' @param composition A string describing whether the resulting `x` and `y`
#'   should be returned as a `"matrix"` or a `"data.frame"`.
#' @param remove_intercept A logical indicating whether to remove the intercept
#'   column after `model.matrix()` is finished.
#' @inheritParams fit.cluster_spec
#' @rdname convert_helpers
#' @keywords internal
.convert_form_to_x_fit <- function(
  formula,
  data,
  ...,
  na.action = na.omit,
  indicators = "traditional",
  composition = "data.frame",
  remove_intercept = TRUE
) {
  if (!(composition %in% c("data.frame", "matrix"))) {
    cli::cli_abort(
      "{.arg composition} should be {.cls data.frame} or {.cls matrix}."
    )
  }

  ## Assemble model.frame call from call arguments
  mf_call <- quote(model.frame(formula, data))
  mf_call$na.action <- match.call()$na.action # TODO this should work better
  dots <- quos(...)
  check_form_dots(dots)
  for (i in seq_along(dots)) {
    mf_call[[names(dots)[i]]] <- get_expr(dots[[i]])
  }

  mod_frame <- rlang::eval_tidy(mf_call)
  mod_terms <- attr(mod_frame, "terms")

  w <- as.vector(model.weights(mod_frame))
  if (!is.null(w) && !is.numeric(w)) {
    cli::cli_abort("The {.arg weights} must be a numeric vector.")
  }

  # TODO: Do we actually use the offset when fitting?
  # Extract any inline offsets specified in the formula from the model frame
  offset <- model.offset(mod_frame)

  if (indicators != "none") {
    if (indicators == "one_hot") {
      local_one_hot_contrasts()
    }

    x <- model.matrix(mod_terms, mod_frame)
  } else {
    # this still ignores -vars in formula
    x <- model.frame(mod_terms, data)
    y_cols <- attr(mod_terms, "response")
    if (identical(y_cols, 0L)) {
      y_cols <- NULL
    }
    if (length(y_cols) > 0) {
      x <- x[, -y_cols, drop = FALSE]
    }
  }

  if (remove_intercept) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }
  options <-
    list(
      indicators = indicators,
      composition = composition,
      remove_intercept = remove_intercept
    )

  if (composition == "data.frame") {
    res <-
      list(
        x = as.data.frame(x),
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        options = options
      )
  } else {
    # Since a matrix is requested, try to convert y but check
    # to see if it is possible
    res <-
      list(
        x = x,
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        options = options
      )
  }
  res
}

check_form_dots <- function(x) {
  good_args <- c("subset", "weights")
  good_names <- names(x) %in% good_args
  if (any(!good_names)) {
    cli::cli_abort(
      c(
        "The argument{?s} {.code {names(x)[!good_names]}} cannot be used
        to create the data.",
        "i" = "Possible arguments are: {.code {good_args}}."
      )
    )
  }
  invisible(NULL)
}

local_one_hot_contrasts <- function(frame = rlang::caller_env()) {
  contrasts <- getOption("contrasts")
  contrasts["unordered"] <- "contr_one_hot"

  rlang::local_options(contrasts = contrasts, .frame = frame)
}

#' @param object An object of class [`cluster_fit`].
#' @inheritParams predict.cluster_fit
#' @rdname convert_helpers
#' @keywords internal
.convert_form_to_x_new <- function(
  object,
  new_data,
  na.action = stats::na.pass,
  composition = "data.frame"
) {
  if (!(composition %in% c("data.frame", "matrix"))) {
    cli::cli_abort(
      "{.arg composition} should be either {.code data.frame} or {.code matrix}."
    )
  }

  mod_terms <- object$terms
  mod_terms <- stats::delete.response(mod_terms)

  new_data <-
    model.frame(
      mod_terms,
      new_data,
      na.action = na.action,
      xlev = object$xlevels
    )

  cl <- attr(mod_terms, "dataClasses")
  if (!is.null(cl)) {
    stats::.checkMFClasses(cl, new_data)
  }

  # TODO: Do we actually use the returned offsets anywhere for prediction?
  # Extract offset from model frame. Multiple offsets will be added together.
  # Offsets might have been supplied through the formula.
  offset <- model.offset(new_data)

  if (object$options$indicators != "none") {
    if (object$options$indicators == "one_hot") {
      local_one_hot_contrasts()
    }

    new_data <- model.matrix(mod_terms, new_data)
  }

  if (object$options$remove_intercept) {
    new_data <- new_data[, colnames(new_data) != "(Intercept)", drop = FALSE]
  }

  if (composition == "data.frame") {
    new_data <- as.data.frame(new_data)
  } else {
    new_data <- as.matrix(new_data)
  }
  list(x = new_data, offset = offset)
}
