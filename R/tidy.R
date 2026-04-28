#' Turn a tidyclust model object into a tidy tibble
#'
#' This method tidies the model in a tidyclust model object, if it exists.
#'
#' @inheritParams generics::tidy
#'
#' @return A tibble with one row per cluster. Columns depend on the underlying
#'   engine but typically include `.cluster` and cluster-level summary
#'   statistics such as centroid coordinates or cluster size.
#'
#' @examples
#' # tidy() support depends on the underlying engine. For the stats engine,
#' # broom must be installed.
#' \dontrun{
#' kmeans_fit <- k_means(num_clusters = 3) |>
#'   set_engine("stats") |>
#'   fit(~., mtcars)
#'
#' tidy(kmeans_fit)
#'
#' hclust_fit <- hier_clust(num_clusters = 3) |>
#'   set_engine("stats") |>
#'   fit(~., mtcars)
#'
#' tidy(hclust_fit)
#' }
#' @export
tidy.cluster_fit <- function(x, ...) generics::tidy(x$fit, ...)

#' Construct a single row summary "glance" of a model, fit, or other object
#'
#' This method glances the model in a tidyclust model object, if it exists.
#'
#' @inheritParams generics::glance
#'
#' @return A one-row tibble with model-level summary statistics such as total
#'   within-cluster sum of squares, between-cluster sum of squares, and number
#'   of iterations. Support depends on the underlying engine.
#'
#' @examples
#' # glance() support depends on the underlying engine.
#' \dontrun{
#' kmeans_fit <- k_means(num_clusters = 3) |>
#'   set_engine("stats") |>
#'   fit(~., mtcars)
#'
#' glance(kmeans_fit)
#' }
#' @export
glance.cluster_fit <- function(x, ...) generics::glance(x$fit, ...)
