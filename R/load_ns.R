#' Quietly load package namespace
#'
#' For one or more packages, load the namespace. This is used during parallel
#' processing since the different parallel backends handle the package
#' environments differently.
#' @param x A character vector of packages.
#' @param infra Should base tidymodels packages be loaded as well?
#' @return An invisible NULL.
#' @keywords internal
#' @export
load_pkgs.cluster_spec <- function(x, infra = TRUE, ...) {
  pkgs <- required_pkgs(x)
  if (infra) {
    pkgs <- c(infra_pkgs, pkgs)
  }
  tune::.load_namespace(unique(pkgs))
}

infra_pkgs <- c(
  "tune",
  "recipes",
  "tidyclust",
  "yardstick",
  "purrr",
  "dplyr",
  "tibble",
  "dials",
  "rsample",
  "workflows",
  "tidyr",
  "rlang",
  "vctrs"
)
