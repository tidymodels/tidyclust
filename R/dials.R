#' Number of Clusters
#'
#' @inheritParams dials::Laplace
#' @examples
#' num_clusters()
#' @export
num_clusters <- function(range = c(1L, 10L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_clusters = "# Clusters"),
    finalize = NULL
  )
}
