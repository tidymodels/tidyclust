# https://github.com/tidymodels/parsnip/blob/main/R/extract.R

#' Extract elements of a tidyclust model object
#'
#' @description
#' These functions extract various elements from a clustering object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a tidyclust model fit. For example, when using [tidyclust::k_means()]
#'   with the `"lm"` engine, this returns the underlying `kmeans` object.
#'
#' - `extract_parameter_set_dials()` returns a set of dials parameter objects.
#'
#' @param x A [`cluster_fit`] object or a [`cluster_spec`] object.
#' @param ... Not currently used.
#' @details
#' Extracting the underlying engine fit can be helpful for describing the
#'  model (via `print()`, `summary()`, `plot()`, etc.) or for variable
#'  importance/explainers.
#'
#'  However, users should not invoke the
#'  \code{\link[=predict.cluster_fit]{predict()}} method on an extracted model.
#'  There may be preprocessing operations that `tidyclust` has executed on the
#'  data prior to giving it to the model. Bypassing these can lead to errors or
#'  silently generating incorrect predictions.
#'
#' **Good**:
#' ```r
#'    tidyclust_fit |> predict(new_data)
#' ```
#'
#' **Bad**:
#' ```r
#'    tidyclust_fit |> extract_fit_engine() |> predict(new_data)
#' ```
#' @return
#' The extracted value from the tidyclust object, `x`, as described in the
#' description section.
#'
#' @name extract-tidyclust
#' @examples
#' kmeans_spec <- k_means(num_clusters = 2)
#' kmeans_fit <- fit(kmeans_spec, ~., data = mtcars)
#'
#' extract_fit_engine(kmeans_fit)
NULL

#' @rdname extract-tidyclust
#' @export
extract_fit_engine.cluster_fit <- function(x, ...) {
  if (any(names(x) == "fit")) {
    return(x$fit)
  }
  cli::cli_abort("Internal error: The model fit does not have an engine fit.")
}
