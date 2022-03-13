specific_model <- function(x) {
  cls <- class(x)
  cls[!cls %in% c("cluster_spec", "model_spec")]
}

possible_engines <- function(object, ...) {
  m_env <- get_model_env_celery()
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

check_installs <- function(x) {
  if (length(x$method$libs) > 0) {
    is_inst <- purrr::map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      missing_pkg <- x$method$libs[!is_inst]
      missing_pkg <- paste0(missing_pkg, collapse = ", ")
      rlang::abort(
        glue::glue(
          "This engine requires some package installs: ",
          glue::glue_collapse(glue::glue("'{missing_pkg}'"), sep = ", ")
        )
      )
    }
  }
}

# ------------------------------------------------------------------------------

#' Declare a computational engine and specific arguments
#'
#' `set_engine_celery()` is used to specify which package or system will be used
#'  to fit the model, along with any arguments specific to that software.
#'
#' @section Engines:
#' Based on the currently loaded packages, the following lists the set of
#' engines available to each model specification.

#' @param object A model specification.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Any optional arguments associated with the chosen computational
#'  engine. These are captured as quosures and can be `tune()`.
#' @return An updated model specification.
#' @examples
#' # First, set general arguments using the standardized names
#' mod <-
#'   k_means(k = 10) %>%
#'   # now say how you want to fit the model and another other options
#'   set_engine_celery("stats", iter.max = 15)
#' translate_celery(mod, engine = "stats")
#' @export
set_engine_celery <- function(object, engine, ...) {
  mod_type <- class(object)[1]
  if (!inherits(object, "cluster_spec")) {
    rlang::abort("`object` should have class 'cluster_spec'.")
  }

  if (rlang::is_missing(engine)) {
    stop_missing_engine(mod_type)
  }
  object$engine <- engine
  check_spec_mode_engine_val(mod_type, object$engine, object$mode)

  new_cluster_spec(
    cls = mod_type,
    args = object$args,
    eng_args = enquos(...),
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

stop_missing_engine <- function(cls) {
  info <-
    get_from_env_celery(cls) %>%
    dplyr::group_by(mode) %>%
    dplyr::summarize(
      msg = paste0(
        unique(mode), " {",
        paste0(unique(engine), collapse = ", "),
        "}"
      ),
      .groups = "drop"
    )
  if (nrow(info) == 0) {
    rlang::abort(paste0("No known engines for `", cls, "()`."))
  }
  msg <- paste0(info$msg, collapse = ", ")
  msg <- paste("Missing engine. Possible mode/engine combinations are:", msg)
  rlang::abort(msg)
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

