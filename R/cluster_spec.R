# https://github.com/tidymodels/parsnip/blob/main/R/misc.R

#' Functions required for tidyclust-adjacent packages
#'
#' These functions are helpful when creating new packages that will register new
#' cluster specifications.
#'
#' @return A [`cluster_spec`] object made to work with tidyclust.
#'
#' @export
#' @keywords internal
new_cluster_spec <- function(cls, args, eng_args, mode, method, engine) {
  modelenv::check_spec_mode_engine_val(
    model = cls,
    mode = mode,
    eng = engine,
    call = rlang::caller_env()
  )

  out <- list(
    args = args,
    eng_args = eng_args,
    mode = mode,
    method = method,
    engine = engine
  )
  class(out) <- make_classes_tidyclust(cls)
  out <- modelenv::new_unsupervised_spec(out)
  out
}

#' Prepend a new class
#'
#' This adds an extra class to a base class of "cluster_spec".
#'
#' @param prefix A character string for a class.
#' @return A character vector.
#' @keywords internal
#' @export
make_classes_tidyclust <- function(prefix) {
  c(prefix, "cluster_spec")
}
