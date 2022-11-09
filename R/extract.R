#' @export
extract_fit_engine.cluster_fit <- function (x, ...) {
  if (any(names(x) == "fit")) {
    return(x$fit)
  }
  rlang::abort("Internal error: The model fit does not have an engine fit.")
}
