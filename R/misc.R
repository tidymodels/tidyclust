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
check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!rlang::is_empty(terms))
    rlang::abort("Please pass other arguments to the model function via `set_engine()`.")
  terms
}

#' Print helper for model objects
#'
#' A common format function that prints information about the model object (e.g.
#' arguments, calls, packages, etc).
#'
#' @param x A model object.
#' @param ... Not currently used.
#' @keywords internal
#' @export
model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, rlang::lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- purrr::map(non_null_args, convert_arg)
    cat(print_arg_list(non_null_args), "\n", sep = "")
  }
  if (length(x$eng_args) > 0) {
    cat("Engine-Specific Arguments:\n")
    x$eng_args <- purrr::map(x$eng_args, convert_arg)
    cat(print_arg_list(x$eng_args), "\n", sep = "")
  }
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$libs) > 0) {
        if (length(x$method$libs) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$libs, collapse = ", "), "\n")
      }
    }
  }
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

#' Print the model call
#'
#' @param x A "cluster_spec" object.
#' @return A character string.
#' @keywords internal
#' @export
show_call <- function(object) {
  object$method$fit$args <-
    purrr::map(object$method$fit$args, convert_arg)
  if (
    is.null(object$method$fit$func["pkg"]) ||
    is.na(object$method$fit$func["pkg"])
  ) {
    res <- rlang::call2(object$method$fit$func["fun"], !!!object$method$fit$args)
  } else {
    res <-
      rlang::call2(object$method$fit$func["fun"],
                   !!!object$method$fit$args,
                   .ns = object$method$fit$func["pkg"])
  }
  res
}
