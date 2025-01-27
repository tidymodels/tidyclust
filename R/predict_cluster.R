#' Other predict methods.
#'
#' These are internal functions not meant to be directly called by the user.
#'
#' @return A `tibble::tibble()`.
#'
#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict_cluster.cluster_fit
#' @export
predict_cluster <- function(object, ...) {
  UseMethod("predict_cluster")
}

#' @keywords internal
#' @return A `tibble::tibble()`.
#' @rdname other_predict
#' @inheritParams predict.cluster_fit
#' @method predict_cluster cluster_fit
#' @export predict_cluster.cluster_fit
#' @export
predict_cluster.cluster_fit <- function(object, new_data, ...) {
  check_spec_pred_type(object, "cluster")

  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$cluster$pre)) {
    new_data <- object$spec$method$pred$cluster$pre(new_data, object)
  }

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$cluster)
  pred_call <- rlang::call_modify(pred_call, ...)

  res <- rlang::eval_tidy(pred_call)
  # post-process the predictions

  if (!is.null(object$spec$method$pred$cluster$post)) {
    res <- object$spec$method$pred$cluster$post(res, object)
  }

  unname(res)
}
