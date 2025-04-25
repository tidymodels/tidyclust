# https://github.com/tidymodels/parsnip/blob/main/R/fit_helpers.R

form_form <- function(object, control, env, ...) {
  # evaluate quoted args once here to check them
  object <- check_args(object)

  # sub in arguments to actual syntax for corresponding engine
  object <- translate_tidyclust(object, engine = object$engine)

  fit_call <- make_form_call(object, env = env)

  res <- list(
    spec = object
  )

  if (control$verbosity > 1L) {
    elapsed <- system.time(
      res$fit <- eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      ),
      gcFirst = FALSE
    )
  } else {
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = env,
      ...
    )
    elapsed <- list(elapsed = NA_real_)
  }
  res$elapsed <- elapsed
  res
}

form_x <- function(object, control, env, target = "none", ...) {
  encoding_info <-
    modelenv::get_encoding(class(object)[1]) |>
    dplyr::filter(mode == object$mode, engine == object$engine)

  indicators <- encoding_info |> dplyr::pull(predictor_indicators)
  remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)

  data_obj <- .convert_form_to_x_fit(
    formula = env$formula,
    data = env$data,
    ...,
    composition = target,
    indicators = indicators,
    remove_intercept = remove_intercept
  )
  env$x <- data_obj$x

  res <- x_x(
    object = object,
    env = env, # weights!
    control = control,
    target = target
  )
  data_obj$x <- NULL
  data_obj$weights <- NULL
  res$preproc <- data_obj
  res
}

x_x <- function(object, env, control, target = "none", y = NULL, ...) {
  if (!is.null(y) && length(y) > 0) {
    cli::cli_abort("Outcomes are not used in {.cls cluster_spec} objects.")
  }
  encoding_info <-
    modelenv::get_encoding(class(object)[1]) |>
    dplyr::filter(mode == object$mode, engine == object$engine)

  remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)
  if (remove_intercept) {
    env$x <- env$x[, colnames(env$x) != "(Intercept)", drop = FALSE]
  }

  # evaluate quoted args once here to check them
  object <- check_args(object)

  # sub in arguments to actual syntax for corresponding engine
  object <- translate_tidyclust(object, engine = object$engine)

  fit_call <- make_x_call(object, target)

  res <- list(spec = object)

  if (control$verbosity > 1L) {
    elapsed <- system.time(
      res$fit <- eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      ),
      gcFirst = FALSE
    )
  } else {
    res$fit <- eval_mod(
      fit_call,
      capture = control$verbosity == 0,
      catch = control$catch,
      env = env,
      ...
    )
    elapsed <- list(elapsed = NA_real_)
  }

  res$elapsed <- elapsed
  res
}

x_form <- function(object, env, control, ...) {
  encoding_info <-
    modelenv::get_encoding(class(object)[1]) |>
    dplyr::filter(mode == object$mode, engine == object$engine)

  remove_intercept <- encoding_info |> dplyr::pull(remove_intercept)

  data_obj <-
    .convert_x_to_form_fit(
      x = env$x,
      weights = NULL,
      remove_intercept = remove_intercept
    )
  env$formula <- data_obj$formula
  env$data <- data_obj$data

  # which terms etc goes in the preproc slot here?
  res <- form_form(
    object = object,
    env = env,
    control = control,
    ...
  )
  res$preproc <- data_obj[c("x_var")]
  res
}
