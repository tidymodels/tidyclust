# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

#' @export
check_args.default <- function(object) {
  invisible(object)
}

check_eng_args_valid <- function(object, valid_eng_args) {
  eng_args <- names(object$eng_args)
  if (length(eng_args) == 0) {
    return(invisible(NULL))
  }
  unknown <- setdiff(eng_args, valid_eng_args)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown engine {cli::qty(unknown)}argument{?s} for engine \\
        {.val {object$engine}}: {.arg {unknown}}.",
        "i" = "Valid arguments are: {.arg {valid_eng_args}}."
      ),
      call = call("set_engine")
    )
  }
  invisible(NULL)
}

extract_training_data <- function(object) {
  if (inherits(object, "workflow")) {
    object <- object$fit$fit$fit
  } else if (inherits(object, "cluster_fit")) {
    object <- object$fit
  }
  attr(object, "training_data")
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
