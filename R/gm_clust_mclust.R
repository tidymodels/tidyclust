#' Gaussian Mixture Model (GMM) via mclust
#'
#' [gm_clust()] creates GMM model.
#'
#' @includeRmd man/rmd/gm_clust_mclust.md details
#'
#' @name details_gm_clust_mclust
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed

# predict.Mclust uses object$data only to check ncol(), so we replace it with
# a zero-row matrix preserving column structure.
# @export - onLoad
axe_data.Mclust <- function(x, verbose = FALSE, ...) {
  x$data <- x$data[0L, , drop = FALSE]
  attr(x, "training_data") <- NULL
  add_butcher_class(x)
}

# @export - onLoad
axe_fitted.Mclust <- function(x, verbose = FALSE, ...) {
  x$z <- matrix(numeric(0), nrow = 0, ncol = ncol(x$z))
  x$classification <- integer(0)
  x$uncertainty <- numeric(0)
  add_butcher_class(x)
}
