# https://github.com/tidymodels/parsnip/blob/main/R/engines.R

#' Change engine of a cluster specification
#'
#' @inheritParams parsnip::set_engine
#' @return An updated [`cluster_spec`] object.
#' @export
set_engine.cluster_spec <- function(object, engine, ...) {
  mod_type <- class(object)[1]

  if (rlang::is_missing(engine)) {
    stop_missing_engine(mod_type)
  }
  object$engine <- engine
  modelenv::check_spec_mode_engine_val(
    model = mod_type,
    mode = object$mode,
    eng = object$engine
  )

  new_cluster_spec(
    cls = mod_type,
    args = object$args,
    eng_args = enquos(...),
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

stop_missing_engine <- function(cls, call = rlang::caller_env()) {
  info <-
    modelenv::get_from_env(cls) |>
    dplyr::group_by(mode) |>
    dplyr::summarize(
      msg = paste0(
        unique(mode),
        " {",
        paste0(unique(engine), collapse = ", "),
        "}"
      ),
      .groups = "drop"
    )
  if (nrow(info) == 0) {
    cli::cli_abort("No known engines for {.fn {cls}}.", call = call)
  }
  cli::cli_abort(
    c(
      "Missing engine.",
      "i" = "Possible mode/engine combinations are: {info$msg}."
    ),
    call = call
  )
}

load_libs <- function(x, quiet, attach = FALSE) {
  for (pkg in x$method$libs) {
    if (!attach) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = quiet))
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  invisible(x)
}

specific_model <- function(x) {
  cls <- class(x)
  cls[!cls %in% c("cluster_spec", "unsupervised_spec")]
}

possible_engines <- function(object, ...) {
  m_env <- modelenv::get_model_env()
  engs <- rlang::env_get(m_env, specific_model(object))
  unique(engs$engine)
}

shhhh <- function(x) {
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))
}

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

check_installs <- function(x, call = rlang::caller_env()) {
  if (length(x$method$libs) > 0) {
    is_inst <- map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      missing_pkg <- x$method$libs[!is_inst]
      missing_pkg <- paste0(missing_pkg, collapse = ", ")
      cli::cli_abort(
        "This engine requires installing {.pkg {missing_pkg}}.",
        call = call
      )
    }
  }
}
