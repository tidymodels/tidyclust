check_eng_args <- function(args, obj, core_args) {
  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata or arg_key
  protected_args <- unique(c(obj$protect, core_args))
  common_args <- intersect(protected_args, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    common_args <- paste0(common_args, collapse = ", ")
    rlang::warn(glue::glue(
      "The following arguments cannot be manually modified ",
      "and were removed: {common_args}."
    ))
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

  object$method$fit$args[[unname(data_args["x"])]] <-
    switch(target,
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
    fit_args[[unname(data_args[i])]] <- sym(names(data_args)[i])
  }

  # sub in actual formula
  fit_args[[unname(data_args["formula"])]] <- env$formula

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    fit_args
  )
  fit_call
}
