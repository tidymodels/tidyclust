#' Construct a new clustering metric function
#'
#' @description
#' These functions provide convenient wrappers to create the one type of
#' metric functions in celrry: clustering metrics. They add a metric-specific
#'  class to `fn`. These features are used by [cluster_metric_set()]
#' and by [tune_cluster()] when tuning.
#'
#' @param fn A function.
#' @export
new_cluster_metric <- function(fn) {
  if (!is.function(fn)) {
    rlang::abort("`fn` must be a function.")
  }

  class <- c("clust_metric", "metric", "function")

  structure(
    fn,
    class = class
  )
}
