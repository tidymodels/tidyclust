# https://github.com/tidymodels/parsnip/blob/main/R/fit.R

#' Fit a Model Specification to a Data Set
#'
#' `fit()` and `fit_xy()` take a model specification, translate_tidyclust the
#' required code by substituting arguments, and execute the model fit routine.
#'
#' @param object An object of class [`cluster_spec`] that has a chosen engine
#'   (via [set_engine()]).
#' @param formula An object of class `formula` (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted.
#' @param data Optional, depending on the interface (see Details below). A data
#'   frame containing all relevant variables (e.g. predictors, case weights,
#'   etc). Note: when needed, a \emph{named argument} should be used.
#' @param control A named list with elements `verbosity` and `catch`. See
#'   [control_cluster()].
#' @param ... Not currently used; values passed here will be ignored. Other
#'   options required to fit the model should be passed using `set_engine()`.
#' @details  `fit()` and `fit_xy()` substitute the current arguments in the
#'   model specification into the computational engine's code, check them for
#'   validity, then fit the model using the data and the engine-specific code.
#'   Different model functions have different interfaces (e.g. formula or
#'   `x`/`y`) and these functions translate_tidyclust between the interface used
#'   when `fit()` or `fit_xy()` was invoked and the one required by the
#'   underlying model.
#'
#'   When possible, these functions attempt to avoid making copies of the data.
#'   For example, if the underlying model uses a formula and `fit()` is invoked,
#'   the original data are references when the model is fit. However, if the
#'   underlying model uses something else, such as `x`/`y`, the formula is
#'   evaluated and the data are converted to the required format. In this case,
#'   any calls in the resulting model objects reference the temporary objects
#'   used to fit the model.
#'
#'   If the model engine has not been set, the model's default engine will be
#'   used (as discussed on each model page). If the `verbosity` option of
#'   [control_cluster()] is greater than zero, a warning will be produced.
#'
#'   If you would like to use an alternative method for generating contrasts
#'   when supplying a formula to `fit()`, set the global option `contrasts` to
#'   your preferred method. For example, you might set it to: `options(contrasts
#'   = c(unordered = "contr.helmert", ordered = "contr.poly"))`. See the help
#'   page for [stats::contr.treatment()] for more possible contrast types.
#' @examples
#' library(dplyr)
#'
#' kmeans_mod <- k_means(num_clusters = 5)
#'
#' using_formula <-
#'   kmeans_mod |>
#'   set_engine("stats") |>
#'   fit(~., data = mtcars)
#'
#' using_x <-
#'   kmeans_mod |>
#'   set_engine("stats") |>
#'   fit_xy(x = mtcars)
#'
#' using_formula
#' using_x
#' @return A [`cluster_fit`] object that contains several elements:
#'   \itemize{
#'     \item \code{spec}: The model specification object (\code{object} in the
#'                        call to \code{fit})
#'     \item \code{fit}: when the model is executed without error, this is the
#'                       model object. Otherwise, it is a \code{try-error}
#'                       object with the error message.
#'     \item \code{preproc}: any objects needed to convert between a formula and
#'                           non-formula interface
#'                           (such as the \code{terms} object)
#'   }
#'   The return value will also have a class related to the fitted model (e.g.
#'   `"_kmeans"`) before the base class of `"cluster_fit"`.
#'
#' @seealso [set_engine()], [control_cluster()], [`cluster_spec`],
#'   [`cluster_fit`]
#' @param x A matrix, sparse matrix, or data frame of predictors. Only some
#'   models have support for sparse matrix input. See `modelenv::get_encoding()`
#'   for details. `x` should have column names.
#' @param case_weights An optional classed vector of numeric case weights. This
#'   must return `TRUE` when [hardhat::is_case_weights()] is run on it. See
#'   [hardhat::frequency_weights()] and [hardhat::importance_weights()] for
#'   examples.
#' @rdname fit
#' @return A fitted [`cluster_fit`] object.
#' @export
#' @export fit.cluster_spec
fit.cluster_spec <- function(
  object,
  formula,
  data,
  control = control_cluster(),
  ...
) {
  if (object$mode == "unknown") {
    cli::cli_abort("Please set the mode in the model specification.")
  }

  control <- parsnip::condense_control(control, control_cluster())

  dots <- quos(...)
  if (is.null(object$engine)) {
    eng_vals <- possible_engines(object)
    object$engine <- eng_vals[1]
    if (control$verbosity > 0) {
      cli::cli_warn("Engine set to {.code {object$engine}}.")
    }
  }

  if (all(c("x", "y") %in% names(dots))) {
    cli::cli_abort(
      "The {.fn fit.cluster_spec} function is for the formula methods. 
      Use {.fn fit_xy} instead."
    )
  }
  cl <- match.call(expand.dots = TRUE)
  # Create an environment with the evaluated argument objects. This will be
  # used when a model call is made later.
  eval_env <- rlang::env()

  eval_env$data <- data
  eval_env$formula <- formula
  fit_interface <-
    check_interface(eval_env$formula, eval_env$data, cl, object)

  # populate `method` with the details for this model type
  object <- add_methods(object, engine = object$engine)

  check_installs(object)

  interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

  # Now call the wrappers that transition between the interface
  # called here ("fit" interface) that will direct traffic to
  # what the underlying model uses. For example, if a formula is
  # used here, `fit_interface_formula` will determine if a
  # translation has to be made if the model interface is x/y/
  res <-
    switch(
      interfaces,
      # homogeneous combinations:
      formula_formula = form_form(
        object = object,
        control = control,
        env = eval_env
      ),

      # heterogenous combinations
      formula_matrix = form_x(
        object = object,
        control = control,
        env = eval_env,
        target = object$method$fit$interface,
        ...
      ),
      formula_data.frame = form_x(
        object = object,
        control = control,
        env = eval_env,
        target = object$method$fit$interface,
        ...
      ),
      cli::cli_abort("{interfaces} is unknown.")
    )
  model_classes <- class(res$fit)
  class(res) <- c(paste0("_", model_classes[1]), "cluster_fit")
  res <- modelenv::new_unsupervised_fit(res)
  res
}

check_interface <- function(formula, data, cl, model) {
  inher(formula, "formula", cl)

  # Determine the `fit()` interface
  form_interface <- !is.null(formula) & !is.null(data)

  if (form_interface) {
    return("formula")
  }
  cli::cli_abort("Error when checking the interface.")
}

inher <- function(x, cls, cl) {
  if (!is.null(x) && !inherits(x, cls)) {
    call <- match.call()
    obj <- deparse(call[["x"]])
    if (length(cls) > 1) {
      cli::cli_abort("{.code {obj}} should be {.cls {cls}}.")
    } else {
      cli::cli_abort("{.code {obj}} should be {.obj_type_friendly {cls}}.")
    }
  }
  invisible(x)
}

add_methods <- function(x, engine) {
  x$engine <- engine
  modelenv::check_spec_mode_engine_val(
    model = class(x)[1],
    mode = x$mode,
    eng = x$engine
  )
  x$method <- get_cluster_spec(specific_model(x), x$mode, x$engine)
  x
}

# ------------------------------------------------------------------------------

eval_mod <- function(e, capture = FALSE, catch = FALSE, ...) {
  if (capture) {
    if (catch) {
      junk <- capture.output(
        res <- try(rlang::eval_tidy(e, ...), silent = TRUE)
      )
    } else {
      junk <- capture.output(res <- rlang::eval_tidy(e, ...))
    }
  } else {
    if (catch) {
      res <- try(rlang::eval_tidy(e, ...), silent = TRUE)
    } else {
      res <- rlang::eval_tidy(e, ...)
    }
  }
  res
}

# ------------------------------------------------------------------------------

#' @rdname fit
#' @export
#' @export fit_xy.cluster_spec
fit_xy.cluster_spec <-
  function(object, x, case_weights = NULL, control = control_cluster(), ...) {
    control <- parsnip::condense_control(control, control_cluster())

    if (is.null(colnames(x))) {
      cli::cli_abort("{.arg x} should have column names.")
    }

    if (is.null(object$engine)) {
      eng_vals <- possible_engines(object)
      object$engine <- eng_vals[1]
      if (control$verbosity > 0) {
        cli::cli_warn("Engine set to {.code {object$engine}}.")
      }
    }

    cl <- match.call(expand.dots = TRUE)
    eval_env <- rlang::env()
    eval_env$x <- x
    fit_interface <- check_x_interface(eval_env$x, cl, object)

    # populate `method` with the details for this model type
    object <- add_methods(object, engine = object$engine)

    check_installs(object)

    interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

    # Now call the wrappers that transition between the interface
    # called here ("fit" interface) that will direct traffic to
    # what the underlying model uses. For example, if a formula is
    # used here, `fit_interface_formula` will determine if a
    # translation has to be made if the model interface is x/y/
    res <-
      switch(
        interfaces,
        # homogeneous combinations:
        matrix_matrix = ,
        data.frame_matrix = x_x(
          object = object,
          env = eval_env,
          control = control,
          target = "matrix",
          ...
        ),
        data.frame_data.frame = ,
        matrix_data.frame = x_x(
          object = object,
          env = eval_env,
          control = control,
          target = "data.frame",
          ...
        ),

        # heterogenous combinations
        matrix_formula = ,
        data.frame_formula = x_form(
          object = object,
          env = eval_env,
          control = control,
          ...
        ),
        cli::cli_abort("{interfaces} is unknown.")
      )
    model_classes <- class(res$fit)
    class(res) <- c(paste0("_", model_classes[1]), "cluster_fit")
    res
  }

check_x_interface <- function(x, cl, model) {
  sparse_ok <- allow_sparse(model)
  sparse_x <- inherits(x, "dgCMatrix")
  if (!sparse_ok && sparse_x) {
    cli::cli_abort(
      "Sparse matrices not supported by this model/engine combination."
    )
  }

  if (sparse_ok) {
    inher(x, c("data.frame", "matrix", "dgCMatrix"), cl)
  } else {
    inher(x, c("data.frame", "matrix"), cl)
  }

  if (sparse_ok) {
    matrix_interface <- !is.null(x) && (is.matrix(x) | sparse_x)
  } else {
    matrix_interface <- !is.null(x) && is.matrix(x)
  }

  df_interface <- !is.null(x) && is.data.frame(x)

  if (matrix_interface) {
    return("matrix")
  }
  if (df_interface) {
    return("data.frame")
  }
  cli::cli_abort("Error when checking the interface")
}

allow_sparse <- function(x) {
  res <- modelenv::get_from_env(paste0(class(x)[1], "_encoding"))
  all(res$allow_sparse_x[res$engine == x$engine])
}
