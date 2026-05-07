#' Cut Height
#'
#' Used in most `tidyclust::hier_clust()` models.
#'
#' @inheritParams dials::Laplace
#' @return A `dials` parameter object for use with [tune::tune_grid()] and
#'   related functions.
#' @examples
#' cut_height()
#' @export
cut_height <- function(range = c(0, dials::unknown()), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(cut_height = "Cut Height"),
    finalize = NULL
  )
}

#' Bandwidth
#'
#' The kernel bandwidth used by mean shift to estimate the local density
#' gradient. Smaller values yield more clusters, while larger values merge
#' them.
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' Used in `tidyclust::mean_shift()` models. The scale on which the bandwidth
#' is interpreted depends on the engine, since some engines rescale predictors
#' internally before applying the kernel.
#'
#' @return A `dials` parameter object for use with [tune::tune_grid()] and
#'   related functions.
#' @examples
#' bandwidth()
#' @export
bandwidth <- function(range = c(0.01, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, TRUE),
    trans = trans,
    label = c(bandwidth = "Bandwidth"),
    finalize = NULL
  )
}

#' The agglomeration Linkage method
#'
#' @param values A character string of possible values. See `linkage_methods`
#'  in examples below.
#'
#' @details
#' This parameter is used in `tidyclust` models for `hier_clust()`.
#' @return A `dials` parameter object for use with [tune::tune_grid()] and
#'   related functions.
#' @examples
#' values_linkage_method
#' linkage_method()
#' @export
linkage_method <- function(values = values_linkage_method) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(linkage_method = "Linkage Method"),
    finalize = NULL
  )
}

#' @rdname linkage_method
#' @export
values_linkage_method <- c(
  "ward.D",
  "ward.D2",
  "single",
  "complete",
  "average",
  "mcquitty",
  "median",
  "centroid"
)
