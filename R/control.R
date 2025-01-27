# https://github.com/tidymodels/tune/blob/main/R/control.R

#' Control the fit function
#'
#' Options can be passed to the [fit.cluster_spec()] function that control the
#' output and computations.
#'
#' @param verbosity An integer where a value of zero indicates that no messages
#'   or output should be shown when packages are loaded or when the model is
#'   fit. A value of 1 means that package loading is quiet but model fits can
#'   produce output to the screen (depending on if they contain their own
#'   `verbose`-type argument). A value of 2 or more indicates that any output
#'   should be seen.
#' @param catch A logical where a value of `TRUE` will evaluate the model inside
#'   of `try(, silent = TRUE)`. If the model fails, an object is still returned
#'   (without an error) that inherits the class "try-error".
#' @return An S3 object with class "control_cluster" that is a named list with
#'   the results of the function call
#' @examples
#' control_cluster()
#'
#' control_cluster(catch = TRUE)
#' @export
control_cluster <- function(verbosity = 1L, catch = FALSE) {
  res <- list(verbosity = verbosity, catch = catch)
  res <- check_control(res)
  class(res) <- "control_cluster"
  res
}

check_control <- function(x, call = rlang::caller_env()) {
  # based on ?is.integer
  int_check <- function(x, tol = .Machine$double.eps^0.5) {
    if (!is.numeric(x)) {
      return(FALSE)
    }
    abs(x - round(x)) < tol
  }
  if (!int_check(x$verbosity)) {
    cli::cli_abort("verbosity should be an integer.", call = call)
  }
  if (!is.logical(x$catch)) {
    cli::cli_abort("catch should be a logical.", call = call)
  }
  x
}

#' @export
print.control_cluster <- function(x, ...) {
  cat("tidyclust control object\n")
  if (x$verbosity > 1) {
    cat(" - verbose level", x$verbosity, "\n")
  }
  if (x$catch) {
    cat(" - fit errors will be caught\n")
  }
  invisible(x)
}
