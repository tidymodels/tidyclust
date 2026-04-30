#' Axing a cluster_fit.
#'
#' cluster_fit objects are created from the tidyclust package.
#'
#' @inheritParams butcher::butcher
#'
#' @return Axed cluster_fit object.
#'
#' @examplesIf rlang::is_installed("butcher")
#' k_fit <- k_means(num_clusters = 3) |>
#'   parsnip::set_engine("stats") |>
#'   fit(~., data = mtcars)
#'
#' butcher::butcher(k_fit)
#'
#' @name axe-cluster_fit
NULL

# @export - onLoad
#' @rdname axe-cluster_fit
axe_call.cluster_fit <- function(x, verbose = FALSE, ...) {
  x$fit <- butcher::axe_call(x$fit, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname axe-cluster_fit
axe_ctrl.cluster_fit <- function(x, verbose = FALSE, ...) {
  x$fit <- butcher::axe_ctrl(x$fit, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname axe-cluster_fit
axe_data.cluster_fit <- function(x, verbose = FALSE, ...) {
  x$fit <- butcher::axe_data(x$fit, verbose = verbose, ...)
  attr(x$fit, "training_data") <- NULL
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname axe-cluster_fit
axe_env.cluster_fit <- function(x, verbose = FALSE, ...) {
  x$fit <- butcher::axe_env(x$fit, verbose = verbose, ...)
  if (inherits(x$preproc$terms, "terms")) {
    attr(x$preproc$terms, ".Environment") <- rlang::base_env()
  }
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname axe-cluster_fit
axe_fitted.cluster_fit <- function(x, verbose = FALSE, ...) {
  x$fit <- butcher::axe_fitted(x$fit, verbose = verbose, ...)
  add_butcher_class(x)
}

# butcher:::add_butcher_class
add_butcher_class <- function(x) {
  if (!any(grepl("butcher", class(x)))) {
    class(x) <- append(paste0("butchered_", class(x)[1]), class(x))
  }
  x
}
