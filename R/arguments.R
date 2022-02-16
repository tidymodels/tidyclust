#' Functions required for celery-adjacent packages
#'
#' These functions are helpful when creating new packages that will register
#' new model specifications.
#' @export
#' @keywords internal
#' @rdname add_on_exports
null_value <- function(x) {
  if (rlang::is_quosure(x)) {
    res <- isTRUE(all.equal(rlang::get_expr(x), rlang::expr(NULL)))
  } else {
    res <- isTRUE(all.equal(x, NULL))
  }
  res
}

check_eng_args <- function(args, obj, core_args) {
  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata or arg_key
  protected_args <- unique(c(obj$protect, core_args))
  common_args <- intersect(protected_args, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    common_args <- paste0(common_args, collapse = ", ")
    rlang::warn(glue::glue("The following arguments cannot be manually modified ",
                           "and were removed: {common_args}."))
  }
  args
}

make_x_call <- function(object, target) {
  fit_args <- object$method$fit$args

  # Get the arguments related to data:
  if (is.null(object$method$fit$data)) {
    data_args <- c(x = "x")
  } else {
    data_args <- object$method$fit$data
  }

  object$method$fit$args[[ unname(data_args["x"]) ]] <-
    switch(
      target,
      none = rlang::expr(x),
      data.frame = rlang::expr(maybe_data_frame(x)),
      matrix = rlang::expr(maybe_matrix(x)),
      rlang::abort(glue::glue("Invalid data type target: {target}."))
    )

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  fit_call
}

# ------------------------------------------------------------------------------

# In some cases, a model function that we are calling has non-standard argument
# names. For example, a function foo() that only has the x/y interface might
# have a signature like `foo(X, Y)`.

# To deal with this, we allow for the `data` element of the model
# as an option to specify these actual argument names
#
#   value = list(
#     interface = "xy",
#     data = c(x = "X", y = "Y"),
#     protect = c("X", "Y"),
#     func = c(pkg = "bar", fun = "foo"),
#     defaults = list()
#   )

#' Make a celery call expression
#'
#' @param fun A character string of a function name.
#' @param ns A character string of a package name.
#' @param args A named list of argument values.
#' @details The arguments are spliced into the `ns::fun()` call. If they are
#' missing, null, or a single logical, then are not spliced.
#' @return A call.
#' @keywords internal
#' @export
make_call <- function(fun, ns, args, ...) {
  # remove any null or placeholders (`missing_args`) that remain
  discard <-
    vapply(args, function(x)
      is_missing_arg(x) | is.null(x), logical(1))
  args <- args[!discard]

  if (!is.null(ns) & !is.na(ns)) {
    out <- rlang::call2(fun, !!!args, .ns = ns)
  } else
    out <- rlang::call2(fun, !!!args)
  out
}

is_missing_arg <- function(x)
  identical(x, quote(missing_arg()))

make_form_call <- function(object, env = NULL) {
  fit_args <- object$method$fit$args

  # Get the arguments related to data:
  if (is.null(object$method$fit$data)) {
    data_args <- c(formula = "formula", data = "data")
  } else {
    data_args <- object$method$fit$data
  }

  # add data arguments
  for (i in seq_along(data_args)) {
    fit_args[[ unname(data_args[i]) ]] <- sym(names(data_args)[i])
  }

  # sub in actual formula
  fit_args[[ unname(data_args["formula"]) ]]  <- env$formula

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    fit_args
  )
  fit_call
}
