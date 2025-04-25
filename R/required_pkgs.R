# https://github.com/tidymodels/parsnip/blob/main/R/required_pkgs.R
#' @export
required_pkgs.cluster_spec <- function(x, infra = TRUE, ...) {
  if (is.null(x$engine)) {
    cli::cli_abort("Please set an engine.")
  }
  get_pkgs(x, infra)
}

#' @export
required_pkgs.cluster_fit <- function(x, infra = TRUE, ...) {
  get_pkgs(x$spec, infra)
}

get_pkgs <- function(x, infra) {
  cls <- class(x)[1]
  pkgs <-
    modelenv::get_from_env(paste0(cls, "_pkgs")) |>
    dplyr::filter(engine == x$engine)
  res <- pkgs$pkg[[1]]
  if (length(res) == 0) {
    res <- character(0)
  }
  if (infra) {
    infra_pkgs <- c("tidyclust")
    res <- c(infra_pkgs, res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}
