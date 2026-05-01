#' K-means via ClusterR
#'
#' [k_means()] creates K-means model. This engine uses the classical definition
#' of a K-means model, which only takes numeric predictors.
#'
#' @includeRmd man/rmd/k_means_ClusterR.md details
#'
#' @name details_k_means_ClusterR
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed

# @export - onLoad
axe_data.KMeansCluster <- function(x, verbose = FALSE, ...) {
  attr(x, "training_data") <- NULL
  add_butcher_class(x)
}

# @export - onLoad
axe_fitted.KMeansCluster <- function(x, verbose = FALSE, ...) {
  x$clusters <- numeric(0)
  add_butcher_class(x)
}
