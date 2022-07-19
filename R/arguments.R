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

#' @export
set_args.cluster_spec <- function(object, ...) {
  the_dots <- enquos(...)
  if (length(the_dots) == 0)
    rlang::abort("Please pass at least one named argument.")
  main_args <- names(object$args)
  new_args <- names(the_dots)
  for (i in new_args) {
    if (any(main_args == i)) {
      object$args[[i]] <- the_dots[[i]]
    } else {
      object$eng_args[[i]] <- the_dots[[i]]
    }
  }
  new_cluster_spec(
    cls = class(object)[1],
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' @export
set_mode_tidyclust <- function(object, mode) {
  cls <- class(object)[1]
  if (rlang::is_missing(mode)) {
    spec_modes <- rlang::env_get(
      get_model_env_tidyclust(),
      paste0(cls, "_modes")
    )
    stop_incompatible_mode(spec_modes, cls = cls)
  }
  check_spec_mode_engine_val(cls, object$engine, mode)
  object$mode <- mode
  object
}
