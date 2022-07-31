#' Resolve a Model Specification for a Computational Engine
#'
#' `translate_tidyclust()` will translate_tidyclust a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translate_tidyclusts generic parameters to their counterparts.
#'
#' @param x A model specification.
#' @param engine The computational engine for the model (see `?set_engine`).
#' @param ... Not currently used.
#' @details
#' `translate_tidyclust()` produces a _template_ call that lacks the specific
#'  argument values (such as `data`, etc). These are filled in once
#'  `fit()` is called with the specifics of the data for the model.
#'  The call may also include `tune()` arguments if these are in
#'  the specification. To handle the `tune()` arguments, you need to use the
#'  [tune package](https://tune.tidymodels.org/). For more information
#'  see <https://www.tidymodels.org/start/tuning/>
#'
#' It does contain the resolved argument names that are specific to
#'  the model fitting function/engine.
#'
#' This function can be useful when you need to understand how
#'  `tidyclust` goes from a generic model specific to a model fitting
#'  function.
#'
#' **Note**: this function is used internally and users should only use it
#'  to understand what the underlying syntax would be. It should not be used
#'  to modify the cluster specification.
#'
#' @export
translate_tidyclust <- function(x, ...) {
  UseMethod("translate_tidyclust")
}

#' @rdname translate_tidyclust
#' @export
#' @export translate_tidyclust.default
translate_tidyclust.default <- function(x, engine = x$engine, ...) {
  check_empty_ellipse_tidyclust(...)
  if (is.null(engine)) {
    rlang::abort("Please set an engine.")
  }

  mod_name <- specific_model(x)

  x$engine <- engine
  if (x$mode == "unknown") {
    rlang::abort("Model code depends on the mode; please specify one.")
  }

  check_spec_mode_engine_val(class(x)[1], x$engine, x$mode)

  if (is.null(x$method)) {
    x$method <- get_cluster_spec(mod_name, x$mode, engine)
  }

  arg_key <- get_args(mod_name, engine)

  # deharmonize primary arguments
  actual_args <- deharmonize(x$args, arg_key)

  # check secondary arguments to see if they are in the final
  # expression unless there are dots, warn if protected args are
  # being altered
  x$eng_args <- check_eng_args(x$eng_args, x$method$fit, arg_key$original)

  # keep only modified args
  modifed_args <- !map_lgl(actual_args, null_value)
  actual_args <- actual_args[modifed_args]

  # look for defaults if not modified in other
  if (length(x$method$fit$defaults) > 0) {
    in_other <- names(x$method$fit$defaults) %in% names(x$eng_args)
    x$defaults <- x$method$fit$defaults[!in_other]
  }

  # combine primary, eng_args, and defaults
  protected <- lapply(x$method$fit$protect, function(x) rlang::expr(missing_arg()))
  names(protected) <- x$method$fit$protect

  x$method$fit$args <- c(protected, actual_args, x$eng_args, x$defaults)

  x
}

# ------------------------------------------------------------------------------
# new code for revised model data structures

get_cluster_spec <- function(model, mode, engine) {
  m_env <- get_model_env_tidyclust()
  env_obj <- rlang::env_names(m_env)
  env_obj <- grep(model, env_obj, value = TRUE)

  res <- list()
  res$libs <-
    rlang::env_get(m_env, paste0(model, "_pkgs")) %>%
    dplyr::filter(engine == !!engine) %>%
    .[["pkg"]] %>%
    .[[1]]

  res$fit <-
    rlang::env_get(m_env, paste0(model, "_fit")) %>%
    dplyr::filter(mode == !!mode & engine == !!engine) %>%
    dplyr::pull(value) %>%
    .[[1]]

  pred_code <-
    rlang::env_get(m_env, paste0(model, "_predict")) %>%
    dplyr::filter(mode == !!mode & engine == !!engine) %>%
    dplyr::select(-engine, -mode)

  res$pred <- pred_code[["value"]]
  names(res$pred) <- pred_code$type

  res
}

get_args <- function(model, engine) {
  m_env <- get_model_env_tidyclust()
  rlang::env_get(m_env, paste0(model, "_args")) %>%
    dplyr::filter(engine == !!engine) %>%
    dplyr::select(-engine)
}

# to replace harmonize
deharmonize <- function(args, key) {
  if (length(args) == 0) {
    return(args)
  }
  parsn <- tibble::tibble(tidyclust = names(args), order = seq_along(args))
  merged <-
    dplyr::left_join(parsn, key, by = "tidyclust") %>%
    dplyr::arrange(order)
  # TODO correct for bad merge?

  names(args) <- merged$original
  args[!is.na(merged$original)]
}

#' Check to ensure that ellipses are empty
#' @param ... Extra arguments.
#' @return If an error is not thrown (from non-empty ellipses), a NULL list.
#' @keywords internal
#' @export
check_empty_ellipse_tidyclust <- function(...) {
  terms <- quos(...)
  if (!rlang::is_empty(terms)) {
    rlang::abort("Please pass other arguments to the model function via `set_engine()`.")
  }
  terms
}
