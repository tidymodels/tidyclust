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
  load_namespace(unique(pkgs))
}

load_namespace <- function(x) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }

  loaded <- map_lgl(x, isNamespaceLoaded)
  x <- x[!loaded]

  if (length(x) > 0) {
    did_load <- map_lgl(x, requireNamespace, quietly = TRUE)
    if (any(!did_load)) {
      bad <- x[!did_load]
      cli::cli_abort("The package{?s} {.pkg {bad}} could not be loaded.")
    }
  }

  invisible(TRUE)
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
