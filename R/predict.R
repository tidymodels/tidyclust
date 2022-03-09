#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict()` can be used for all types of models and uses the
#'  "type" argument for more specificity.
#'
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'   are "numeric", "class", "prob", "conf_int", "pred_int", "quantile", "time",
#'  "hazard", "survival", or "raw". When `NULL`, `predict()` will choose an
#'  appropriate value based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Arguments to the underlying model's prediction
#'  function cannot be passed here (see `opts`). There are some
#'  `parsnip` related options that can be passed, depending on the
#'  value of `type`. Possible arguments are:
#'  \itemize{
#'     \item `interval`: for `type`s of "survival" and "quantile", should
#'            interval estimates be added, if available? Options are `"none"`
#'            and `"confidence"`.
#'     \item `level`: for `type`s of "conf_int", "pred_int", and "survival"
#'            this is the parameter for the tail area of the intervals
#'            (e.g. confidence level for confidence intervals).
#'            Default value is 0.95.
#'     \item `std_error`: add the standard error of fit or prediction (on
#'            the scale of the linear predictors) for `type`s of "conf_int"
#'            and "pred_int". Default value is `FALSE`.
#'     \item `quantile`: the quantile(s) for quantile regression
#'            (not implemented yet)
#'     \item `time`: the time(s) for hazard and survival probability estimates.
#'  }
#' @details If "type" is not supplied to `predict()`, then a choice
#'  is made:
#'
#'   * `type = "numeric"` for regression models,
#'   * `type = "class"` for classification, and
#'   * `type = "time"` for censored regression.
#'
#' `predict()` is designed to provide a tidy result (see "Value"
#'  section below) in a tibble output format.
#'
#'  ## Interval predictions
#'
#'  When using `type = "conf_int"` and `type = "pred_int"`, the options
#'   `level` and `std_error` can be used. The latter is a logical for an
#'   extra column of standard error values (if available).
#'
#'  ## Censored regression predictions
#'
#' For censored regression, a numeric vector for `time` is required when
#' survival or hazard probabilities are requested. Also, when
#' `type = "linear_pred"`, censored regression models will by default be
#' formatted such that the linear predictor _increases_ with time. This may
#' have the opposite sign as what the underlying model's `predict()` method
#' produces. Set `increasing = FALSE` to suppress this behavior.
#'
#' @return With the exception of `type = "raw"`, the results of
#'  `predict.model_fit()` will be a tibble as many rows in the output
#'  as there are rows in `new_data` and the column names will be
#'  predictable.
#'
#' For numeric results with a single outcome, the tibble will have
#'  a `.pred` column and `.pred_Yname` for multivariate results.
#'
#' For hard class predictions, the column is named `.pred_class`
#'  and, when `type = "prob"`, the columns are `.pred_classlevel`.
#'
#' `type = "conf_int"` and `type = "pred_int"` return tibbles with
#'  columns `.pred_lower` and `.pred_upper` with an attribute for
#'  the confidence level. In the case where intervals can be
#'  produces for class probabilities (or other non-scalar outputs),
#'  the columns will be named `.pred_lower_classlevel` and so on.
#'
#' Quantile predictions return a tibble with a column `.pred`, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.pred` and `.quantile` (and perhaps other columns).
#'
#' Using `type = "raw"` with `predict.model_fit()` will return
#'  the unadulterated results of the prediction function.
#'
#' For censored regression:
#'
#'  * `type = "time"` produces a column `.pred_time`.
#'  * `type = "hazard"` results in a list column `.pred` containing tibbles
#'     with a column `.pred_hazard`.
#'  * `type = "survival"` results in a list column `.pred` containing tibbles
#'     with a `.pred_survival` column.
#'
#' In the case of Spark-based models, since table columns cannot
#'  contain dots, the same convention is used except 1) no dots
#'  appear in names and 2) vectors are never returned but
#'  type-specific prediction functions.
#'
#' When the model fit failed and the error was captured, the
#'  `predict()` function will return the same structure as above but
#'  filled with missing values. This does not currently work for
#'  multivariate models.
#'
#' @examples
#' 1+1
#' @method predict cluster_fit
#' @export predict.cluster_fit
#' @export
predict.cluster_fit <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)

  res <- switch(
    type,
    cluster = predict_cluster(object = object, new_data = new_data, ...),
    rlang::abort(glue::glue("I don't know about type = '{type}'"))
  )

  res <- switch(
    type,
    cluster = format_cluster(res),
    res
  )
  res
}


check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
             partition = "cluster",
             rlang::abort("`type` should be 'cluster'."))
  }
  if (!(type %in% pred_types))
    rlang::abort(
      glue::glue(
        "`type` should be one of: ",
        glue::glue_collapse(pred_types, sep = ", ", last = " and ")
      )
    )
  type
}

format_cluster <- function(x) {

  tibble::tibble(.pred_cluster = unname(x))
}

#' Prepare data based on parsnip encoding information
#' @param object A parsnip model object
#' @param new_data A data frame
#' @return A data frame or matrix
#' @keywords internal
#' @export
prepare_data <- function(object, new_data) {
  fit_interface <- object$spec$method$fit$interface

  pp_names <- names(object$preproc)
  if (any(pp_names == "terms") | any(pp_names == "x_var")) {
    # Translation code
    if (fit_interface == "formula") {
      new_data <- .convert_x_to_form_new(object$preproc, new_data)
    } else {
      new_data <- .convert_form_to_x_new(object$preproc, new_data)$x
    }
  }

  remove_intercept <-
    get_encoding_celery(class(object$spec)[1]) %>%
    dplyr::filter(mode == object$spec$mode, engine == object$spec$engine) %>%
    dplyr::pull(remove_intercept)
  if (remove_intercept & any(grepl("Intercept", names(new_data)))) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of("(Intercept)"))
  }

  switch(
    fit_interface,
    none = new_data,
    data.frame = as.data.frame(new_data),
    matrix = as.matrix(new_data),
    new_data
  )
}

make_pred_call <- function(x) {
  if ("pkg" %in% names(x$func))
    cl <- rlang::call2(x$func["fun"],!!!x$args, .ns = x$func["pkg"])
  else
    cl <- rlang::call2(x$func["fun"],!!!x$args)

  cl
}