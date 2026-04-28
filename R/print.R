# https://github.com/tidymodels/parsnip/blob/main/R/print.R

#' Print a cluster object
#'
#' @param x A [`cluster_fit`] or [`cluster_spec`] object.
#' @param ... Arguments passed to the underlying print method.
#' @return The input `x`, invisibly.
#' @rdname print.cluster_fit
#' @keywords internal
#' @export
print.cluster_fit <- function(x, ...) {
  cat("tidyclust cluster object\n\n")
  if (!is.na(x$elapsed[["elapsed"]])) {
    cat(
      "Fit time: ",
      prettyunits::pretty_sec(x$elapsed[["elapsed"]]),
      "\n"
    )
  }
  if (inherits(x$fit, "try-error")) {
    cat("Cluster fit failed with error:\n", x$fit, "\n")
  } else {
    print(x$fit, ...)
  }
  invisible(x)
}

#' @rdname print.cluster_fit
#' @export
print.cluster_spec <- function(x, ...) {
  cat("Cluster Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  if (!is.null(x$method$fit$args)) {
    cat("Cluster fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}
