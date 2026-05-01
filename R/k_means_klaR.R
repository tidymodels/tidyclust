#' K-means via klaR
#'
#' [k_means()] creates K-Modes model. This model is intended to be used with
#' categorical predictors. Although it will accept numeric predictors if they
#' contain a few number of unique values. The numeric predictors will then be
#' treated like categorical.
#'
#' @includeRmd man/rmd/k_means_klaR.md details
#'
#' @name details_k_means_klaR
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed

# @export - onLoad
axe_data.kmodes <- function(x, verbose = FALSE, ...) {
  attr(x, "training_data") <- NULL
  add_butcher_class(x)
}

# @export - onLoad
axe_fitted.kmodes <- function(x, verbose = FALSE, ...) {
  x$cluster <- integer(0)
  add_butcher_class(x)
}
