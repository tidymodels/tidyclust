# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

check_args.default <- function(object) {
  invisible(object)
}

#' Check to ensure that ellipses are empty
#' @param ... Extra arguments.
#' @return If an error is not thrown (from non-empty ellipses), a NULL list.
#' @keywords internal
#' @export
check_empty_ellipse_celery <- function (...)  {
  terms <- quos(...)
  if (!rlang::is_empty(terms))
    rlang::abort("Please pass other arguments to the model function via `set_engine_celery()`.")
  terms
}

convert_arg <- function(x) {
  if (rlang::is_quosure(x))
    rlang::quo_get_expr(x)
  else
    x
}

print_arg_list <- function(x, ...) {
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  res <- paste0("  ", names(x2), " = ", x2, collaspe = "\n")
  cat(res, sep = "")
}

deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}
