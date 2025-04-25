# https://github.com/tidymodels/parsnip/blob/main/R/predict.R

#' Model predictions
#'
#' Apply to a model to create different types of predictions. `predict()` can be
#' used for all types of models and uses the "type" argument for more
#' specificity.
#'
#' @param object An object of class [`cluster_fit`].
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values are
#'   "cluster", or "raw". When `NULL`, `predict()` will choose an appropriate
#'   value based on the model's mode.
#' @param opts A list of optional arguments to the underlying predict function
#'   that will be used when `type = "raw"`. The list should not include options
#'   for the model object or the new data being predicted.
#' @param ... Arguments to the underlying model's prediction function cannot be
#'   passed here (see `opts`).
#'
#' @details
#'
#' If "type" is not supplied to `predict()`, then a choice is made:
#'
#' * `type = "cluster"` for clustering models
#'
#' `predict()` is designed to provide a tidy result (see "Value" section below)
#' in a tibble output format.
#'
#' The ordering of the clusters is such that the first observation in the
#' training data set will be in cluster 1, the next observation that doesn't
#' belong to cluster 1 will be in cluster 2, and so on and forth. As the
#' ordering of clustering doesn't matter, this is done to avoid identical sets
#' of clustering having different labels if fit multiple times.
#'
#' ## What does it mean to predict?
#'
#' Prediction is not always formally defined for clustering models. Therefore,
#' each [`cluster_spec`] method will have their own section on how "prediction"
#' is interpreted, and done if implemented.
#'
#' ## Related functions
#'
#' `predict()` when used with tidyclust objects is a part of a trio of functions
#' doing similar things:
#'
#' - [extract_cluster_assignment()] returns the cluster assignments of the
#' training observations
#' - [extract_centroids()] returns the location of the centroids
#' - \code{\link[=predict.cluster_fit]{predict()}} returns the cluster a new
#' observation belongs to
#'
#' @return With the exception of `type = "raw"`, the results of
#'   `predict.cluster_fit()` will be a tibble as many rows in the output as
#'   there are rows in `new_data` and the column names will be predictable.
#'
#'   For clustering results the tibble will have a `.pred_cluster` column.
#'
#'   Using `type = "raw"` with `predict.cluster_fit()` will return the
#'   unadulterated results of the prediction function.
#'
#'   When the model fit failed and the error was captured, the `predict()`
#'   function will return the same structure as above but filled with missing
#'   values. This does not currently work for multivariate models.
#'
#' @seealso [extract_cluster_assignment()] [extract_centroids()]
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit |>
#'   predict(new_data = mtcars)
#'
#' # Some models such as `hier_clust()` fits in such a way that you can specify
#' # the number of clusters after the model is fit
#' hclust_spec <- hier_clust() |>
#'   set_engine("stats")
#'
#' hclust_fit <- fit(hclust_spec, ~., mtcars)
#'
#' hclust_fit |>
#'   predict(new_data = mtcars[4:6, ], num_clusters = 2)
#'
#' hclust_fit |>
#'   predict(new_data = mtcars[4:6, ], cut_height = 250)
#' @method predict cluster_fit
#' @export predict.cluster_fit
#' @export
predict.cluster_fit <- function(
  object,
  new_data,
  type = NULL,
  opts = list(),
  ...
) {
  if (inherits(object$fit, "try-error")) {
    cli::cli_warn(
      "Model fit failed; cannot make predictions."
    )
    return(NULL)
  }

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)

  res <- switch(
    type,
    cluster = predict_cluster(object = object, new_data = new_data, ...),
    raw = predict_raw(object = object, new_data = new_data, opts = opts, ...),
    cli::cli_abort("I don't know about type = {.val {type}}")
  )

  res <- switch(type, cluster = format_cluster(res), res)
  res
}

check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(
        object$spec$mode,
        partition = "cluster",
        cli::cli_abort("The {.arg type} argument should be {.val cluster}.")
      )
  }
  if (!(type %in% pred_types)) {
    cli::cli_abort("{.arg type} should be {.or {pred_types}}.")
  }
  type
}

format_cluster <- function(x) {
  tibble::tibble(.pred_cluster = unname(x))
}

prepare_data <- function(object, new_data) {
  fit_interface <- object$spec$method$fit$interface

  pp_names <- names(object$preproc)
  if (any(pp_names == "terms") || any(pp_names == "x_var")) {
    # Translation code
    if (fit_interface == "formula") {
      new_data <- .convert_x_to_form_new(object$preproc, new_data)
    } else {
      new_data <- .convert_form_to_x_new(object$preproc, new_data)$x
    }
  }

  remove_intercept <-
    modelenv::get_encoding(class(object$spec)[1]) |>
    dplyr::filter(mode == object$spec$mode, engine == object$spec$engine) |>
    dplyr::pull(remove_intercept)
  if (remove_intercept && any(grepl("Intercept", names(new_data)))) {
    new_data <- new_data |> dplyr::select(-dplyr::one_of("(Intercept)"))
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
  if ("pkg" %in% names(x$func)) {
    cl <- rlang::call2(x$func["fun"], !!!x$args, .ns = x$func["pkg"])
  } else {
    cl <- rlang::call2(x$func["fun"], !!!x$args)
  }

  cl
}

#' @export
predict.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}
