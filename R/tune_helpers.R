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
  tune::new_bare_tibble(
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
    updater <- tune::.update_recipe
    step_ids <- map_chr(x$steps, \(.x) .x$id)
  } else {
    updater <- \(...) tune::.update_model(..., source = "cluster_spec")
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
