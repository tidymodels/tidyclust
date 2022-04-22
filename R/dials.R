k <- function(range = c(1L, 10L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(k = "# Clusters"),
    finalize = NULL
  )
}
