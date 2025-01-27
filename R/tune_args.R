#' @export
tune_args.cluster_spec <- function(object, full = FALSE, ...) {
  # use the cluster_spec top level class as the id
  cluster_type <- class(object)[1]

  if (length(object$args) == 0L && length(object$eng_args) == 0L) {
    return(tune_tbl())
  }

  # Locate tunable args in spec args and engine specific args
  object$args <- map(object$args, convert_args)
  object$eng_args <- map(object$eng_args, convert_args)

  arg_id <- map_chr(object$args, find_tune_id)
  eng_arg_id <- map_chr(object$eng_args, find_tune_id)
  res <- c(arg_id, eng_arg_id)
  res <- ifelse(res == "", names(res), res)

  tune_tbl(
    name = names(res),
    tunable = unname(!is.na(res)),
    id = res,
    source = "cluster_spec",
    component = cluster_type,
    component_id = NA_character_,
    full = full
  )
}

convert_args <- function(x) {
  if (rlang::is_quosure(x)) {
    x <- rlang::quo_get_expr(x)
  }
  x
}

find_tune_id <- function(x) {
  # STEP 1 - Early exits

  # Early exit for empty elements (like list())
  if (length(x) == 0L) {
    return(NA_character_)
  }

  # turn quosures into expressions before continuing
  if (rlang::is_quosures(x)) {
    # Try to evaluate to catch things in the global envir. If it is a dplyr
    # selector, it will fail to evaluate.
    .x <- try(map(x, rlang::eval_tidy), silent = TRUE)
    if (inherits(.x, "try-error")) {
      x <- map(x, rlang::quo_get_expr)
    } else {
      x <- .x
    }
  }

  id <- tune_id(x)
  if (!is.na(id)) {
    return(id)
  }

  if (is.atomic(x) || is.name(x) || length(x) == 1) {
    return(NA_character_)
  }

  # STEP 2 - Recursion
  tunable_elems <- vector("character", length = length(x))

  # use map_lgl
  for (i in seq_along(x)) {
    tunable_elems[i] <- find_tune_id(x[[i]])
  }

  tunable_elems <- tunable_elems[!is.na(tunable_elems)]
  if (length(tunable_elems) == 0) {
    tunable_elems <- NA_character_
  }

  if (sum(tunable_elems == "", na.rm = TRUE) > 1) {
    cli::cli_abort(
      c(
        "Only one tunable value is currently allowed per argument.",
        "i" = "The current argument has: {.code {paste0(deparse(x), collapse = '')}}."
      )
    )
  }

  return(tunable_elems)
}

tune_id <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  } else {
    if (rlang::is_quosures(x)) {
      # Try to evaluate to catch things in the global envir.
      .x <- try(map(x, rlang::eval_tidy), silent = TRUE)
      if (inherits(.x, "try-error")) {
        x <- map(x, rlang::quo_get_expr)
      } else {
        x <- .x
      }
      if (is.null(x)) {
        return(NA_character_)
      }
    }

    # [tune()] will always return a call object
    if (is.call(x)) {
      if (rlang::call_name(x) == "tune") {
        # If an id was specified:
        if (length(x) > 1) {
          return(x[[2]])
        } else {
          # no id
          return("")
        }
        return(x$id)
      } else {
        return(NA_character_)
      }
    }
  }
  NA_character_
}

tune_tbl <- function(
  name = character(),
  tunable = logical(),
  id = character(),
  source = character(),
  component = character(),
  component_id = character(),
  full = FALSE
) {
  complete_id <- id[!is.na(id)]
  dups <- duplicated(complete_id)
  if (any(dups)) {
    cli::cli_abort(
      "There are duplicate {.code id} values listed in [{.fn tune}]: {.val {unique(complete_id[dups])}}."
    )
  }

  vry_tbl <- tibble::tibble(
    name = as.character(name),
    tunable = as.logical(tunable),
    id = as.character(id),
    source = as.character(source),
    component = as.character(component),
    component_id = as.character(component_id)
  )

  if (!full) {
    vry_tbl <- vry_tbl[vry_tbl$tunable, ]
  }

  vry_tbl
}
