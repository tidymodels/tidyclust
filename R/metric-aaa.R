#' Construct a new clustering metric function
#'
#' @description
#' These functions provide convenient wrappers to create the one type of
#' metric functions in celrry: clustering metrics. They add a metric-specific
#'  class to `fn`. These features are used by [cluster_metric_set()]
#' and by [tune_cluster()] when tuning.
#'
#' @param fn A function.
#'
#' @param direction A string. One of:
#'   - `"maximize"`
#'   - `"minimize"`
#'   - `"zero"`
#'
#' @export
new_cluster_metric <- function(fn, direction) {
  if (!is.function(fn)) {
    rlang::abort("`fn` must be a function.")
  }

  direction <- rlang::arg_match(
    direction,
    values = c("maximize", "minimize", "zero")
  )

  class <- c("cluster_metric", "metric", "function")

  structure(
    fn,
    direction = direction,
    class = class
  )
}

#' Combine metric functions
#'
#' `metric_set()` allows you to combine multiple metric functions together
#' into a new function that calculates all of them at once.
#'
#' @param ... The bare names of the functions to be included in the metric set.
#'
#' @details
#' All functions must be:
#' - Only cluster metrics
#' @export
cluster_metric_set <- function(...) {
  quo_fns <- rlang::enquos(...)
  validate_not_empty(quo_fns)
  fns <- lapply(quo_fns, rlang::eval_tidy)
  validate_inputs_are_functions(fns)
  names(fns) <- vapply(quo_fns, get_quo_label, character(1))
  validate_function_class(fns)
  fn_cls <- class1(fns[[1]])
  if (fn_cls == "cluster_metric") {
    make_cluster_metric_function(fns)
  } else {
    rlang::abort(paste0("Internal error: `validate_function_class()` should have ",
                 "errored on unknown classes."))
  }
}

validate_not_empty <- function(x) {
  if (rlang::is_empty(x)) {
    rlang::abort(
      "`cluster_metric_set()` requires at least 1 function supplied to `...`."
    )
  }
}

validate_inputs_are_functions <- function(fns){
  is_fun_vec <- vapply(fns, rlang::is_function, logical(1))
  all_fns <- all(is_fun_vec)
  if (!all_fns) {
    not_fn <- which(!is_fun_vec)
    not_fn <- paste(not_fn, collapse = ", ")
    stop("All inputs to `cluster_metric_set()` must be functions. ",
         "These inputs are not: (", not_fn, ").", call. = FALSE)
  }
}

get_quo_label <- function(quo) {
  out <- rlang::as_label(quo)
  if (length(out) != 1L) {
    rlang::abort("Internal error: `as_label(quo)` resulted in a character vector of length >1.")
  }
  is_namespaced <- grepl("::", out, fixed = TRUE)
  if (is_namespaced) {
    split <- strsplit(out, "::", fixed = TRUE)[[1]]
    out <- split[[2]]
  }
  out
}

validate_function_class <- function(fns) {
  fn_cls <- vapply(fns, function(fn) class(fn)[1], character(1))
  fn_cls_unique <- unique(fn_cls)
  n_unique <- length(fn_cls_unique)
  if (n_unique == 0L) {
    return(invisible(fns))
  }
  if (n_unique == 1L) {
    if (fn_cls_unique %in% "cluster_metric") {
      return(invisible(fns))
    }
  }

  fn_bad_names <- lapply(fn_cls_unique, function(x) {
    names(fns)[fn_cls == x]
  })
  fn_cls_unique <- gsub("_metric", "", fn_cls_unique)
  fn_cls_unique <- gsub("function", "other", fn_cls_unique)
  fn_cls_other <- fn_cls_unique == "other"
  if (any(fn_cls_other)) {
    fn_cls_other_loc <- which(fn_cls_other)
    fn_other_names <- fn_bad_names[[fn_cls_other_loc]]
    fns_other <- fns[fn_other_names]
    env_names_other <- vapply(
      fns_other,
      function(fn) rlang::env_name(rlang::fn_env(fn)),
      character(1)
    )
    fn_bad_names[[fn_cls_other_loc]] <-
      paste0(fn_other_names, " ", "<", env_names_other, ">")
  }
  fn_pastable <- mapply(
    FUN = function(fn_type, fn_names) {
      fn_names <- paste0(fn_names, collapse = ", ")
      paste0("- ", fn_type, " (", fn_names, ")")
    },
    fn_type = fn_cls_unique,
    fn_names = fn_bad_names,
    USE.NAMES = FALSE
  )
  fn_pastable <- paste0(fn_pastable, collapse = "\n")
  rlang::abort(
    paste0(
      "\nThe combination of metric functions must be:\n",
      "- only clustering metrics\n",
      "The following metric function types are being mixed:\n",
      fn_pastable
    )
  )
}

make_cluster_metric_function <- function(fns) {
  metric_function <- function(object, new_data = NULL) {
    call_args <- quos(
      object = object,
      new_data = new_data
    )
    calls <- lapply(fns, rlang::call2, !!!call_args)
    metric_list <- mapply(FUN = eval_safely, calls, names(calls),
                          SIMPLIFY = FALSE, USE.NAMES = FALSE)
    dplyr::bind_rows(metric_list)
  }
  class(metric_function) <- c("cluster_metric_set",
                              class(metric_function))
  attr(metric_function, "metrics") <- fns
  metric_function
}

eval_safely <- function(expr, expr_nm, data = NULL, env = rlang::caller_env()) {
  tryCatch(expr = {
    rlang::eval_tidy(expr, data = data, env = env)
  }, error = function(e) {
    rlang::abort(paste0("In metric: `", expr_nm, "`\n", conditionMessage(e)))
  })
}

#' @export
as_tibble.cluster_metric_set <- function(x, ...) {
  metrics <- attributes(x)$metrics
  names <- names(metrics)
  metrics <- unname(metrics)
  classes <- map_chr(metrics, class1)
  dplyr::tibble(metric = names, class = classes)
}

class1 <- function(x) {
  class(x)[[1]]
}

#' @export
print.cluster_metric_set <- function(x, ...) {
  info <- dplyr::as_tibble(x)
  print(info)
  invisible(x)
}

extract_post_preprocessor <- function(object, new_data) {
  preprocessor <- hardhat::extract_preprocessor(object)

  if (inherits(preprocessor, "workflow_variables")) {
    new_data <- dplyr::select(new_data, !!preprocessor$predictors)
  } else if (rlang::is_formula(preprocessor)) {
    new_data <- hardhat::mold(preprocessor, new_data)$predictors
  } else if (inherits(preprocessor, "recipe")) {
    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)
  }
  new_data
}
