# new_tibble() currently doesn't strip attributes
# https://github.com/tidyverse/tibble/pull/769
new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
}

is_cataclysmic <- function(x) {
  is_err <- map_lgl(
    x$.metrics,
    inherits,
    c(
      "simpleError",
      "error"
    )
  )
  if (any(!is_err)) {
    is_good <- map_lgl(
      x$.metrics[!is_err],
      \(.x) {
        tibble::is_tibble(.x) &&
          nrow(.x) > 0
      }
    )
    is_err[!is_err] <- !is_good
  }
  all(is_err)
}

# https://github.com/tidymodels/tune/blob/main/R/tune_grid.R
set_workflow <- function(workflow, control) {
  if (control$save_workflow) {
    if (!is.null(workflow$pre$actions$recipe)) {
      w_size <- utils::object.size(workflow$pre$actions$recipe)
      if (w_size / 1024^2 > 5) {
        cli::cli_inform(
          "The workflow being saved contains a recipe, which is {format(w_size, units = 'Mb', 
          digits = 2)} in memory. If this was not intentional, please set the control 
          setting {.code save_workflow = FALSE}."
        )
      }
    }
    workflow
  } else {
    NULL
  }
}

# https://github.com/tidymodels/tune/blob/main/R/tune_results.R
new_tune_results <- function(
  x,
  parameters,
  metrics,
  rset_info,
  ...,
  class = character()
) {
  new_bare_tibble(
    x = x,
    parameters = parameters,
    metrics = metrics,
    rset_info = rset_info,
    ...,
    class = c(class, "tune_results")
  )
}

# https://github.com/tidymodels/tune/blob/main/R/min_grid.R

#' Determine the minimum set of model fits
#'
#' @param x A cluster specification.
#' @param grid A tibble with tuning parameter combinations.
#' @param ... Not currently used.
#' @return A tibble with the minimum tuning parameters to fit and an additional
#' list column with the parameter combinations used for prediction.
#' @export
min_grid.cluster_spec <- function(x, grid, ...) {
  blank_submodels(grid)
}

blank_submodels <- function(grid) {
  grid |>
    dplyr::mutate(
      .submodels = map(seq_along(nrow(grid)), \(x) list())
    ) |>
    dplyr::mutate_if(is.factor, as.character)
}

is_failure <- function(x) {
  inherits(x, "try-error")
}

#' @export
merge.cluster_spec <- function(x, y, ...) {
  merger(x, y, ...)
}

merger <- function(x, y, ...) {
  if (!is.data.frame(y)) {
    cli::cli_abort("The second argument should be a data frame.")
  }
  pset <- hardhat::extract_parameter_set_dials(x)
  if (nrow(pset) == 0) {
    res <- tibble::tibble(x = map(seq_along(nrow(y)), \(.x) x))
    return(res)
  }
  grid_name <- colnames(y)
  if (inherits(x, "recipe")) {
    updater <- update_recipe
    step_ids <- map_chr(x$steps, \(.x) .x$id)
  } else {
    updater <- update_model
    step_ids <- NULL
  }
  if (!any(grid_name %in% pset$id)) {
    res <- tibble::tibble(x = map(seq_along(nrow(y)), \(.x) x))
    return(res)
  }
  y |>
    dplyr::mutate(
      ..object = map(
        seq_along(nrow(y)),
        \(.x) updater(y[.x, ], x, pset, step_ids, grid_name)
      )
    ) |>
    dplyr::select(x = ..object)
}

# https://github.com/tidymodels/tune/blob/main/R/merge.R
update_model <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset |> dplyr::filter(id == i & source == "cluster_spec")
    if (nrow(param_info) > 1) {
      # TODO figure this out and write a better message
      cli::cli_abort("There are too many things.")
    }
    if (nrow(param_info) == 1) {
      if (param_info$component_id == "main") {
        object$args[[param_info$name]] <-
          rlang::as_quosure(grid[[i]], env = rlang::empty_env())
      } else {
        object$eng_args[[param_info$name]] <-
          rlang::as_quosure(grid[[i]], env = rlang::empty_env())
      }
    }
  }
  object
}

# https://github.com/tidymodels/tune/blob/main/R/merge.R
update_recipe <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset |> dplyr::filter(id == i & source == "recipe")
    if (nrow(param_info) == 1) {
      idx <- which(step_id == param_info$component_id)
      # check index
      # should use the contructor but maybe dangerous/difficult
      object$steps[[idx]][[param_info$name]] <- grid[[i]]
    }
  }
  object
}

# ------------------------------------------------------------------------------

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R#L613
has_preprocessor <- function(workflow) {
  has_preprocessor_recipe(workflow) ||
    has_preprocessor_formula(workflow) ||
    has_preprocessor_variables(workflow)
}

has_preprocessor_recipe <- function(workflow) {
  "recipe" %in% names(workflow$pre$actions)
}

has_preprocessor_formula <- function(workflow) {
  "formula" %in% names(workflow$pre$actions)
}

has_preprocessor_variables <- function(workflow) {
  "variables" %in% names(workflow$pre$actions)
}

has_spec <- function(workflow) {
  "model" %in% names(workflow$fit$actions)
}

set_workflow_spec <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

set_workflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}
