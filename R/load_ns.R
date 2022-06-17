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

  loaded <- purrr::map_lgl(x, isNamespaceLoaded)
  x <- x[!loaded]

  if (length(x) > 0) {
    did_load <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
    if (any(!did_load)) {
      bad <- x[!did_load]
      msg <- paste0("'", bad, "'", collapse = ", ")
      rlang::abort(paste("These packages could not be loaded:", msg))
    }
  }

  invisible(TRUE)
}

infra_pkgs <- c(
  "tune", "recipes", "tidyclust", "yardstick", "purrr", "dplyr", "tibble",
  "dials", "rsample", "workflows", "tidyr", "rlang", "vctrs"
)
