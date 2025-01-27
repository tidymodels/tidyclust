# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

#' @export
check_args.default <- function(object) {
  invisible(object)
}

check_spec_pred_type <- function(object, type) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    cli::cli_abort(
      c(
        "No {type} prediction method available for this model.",
        "i" = "{.arg type} should be one of {.val {possible_preds}}."
      )
    )
  }
  invisible(NULL)
}

spec_has_pred_type <- function(object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}
