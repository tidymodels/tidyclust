# https://github.com/tidymodels/tune/blob/main/R/pull.R#L136
append_predictions <- function(
  collection,
  predictions,
  split,
  control,
  .config = NULL
) {
  if (!control$save_pred) {
    return(NULL)
  }
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  predictions <- vctrs::vec_cbind(predictions, labels(split))

  if (!rlang::is_null(.config)) {
    by <- setdiff(names(.config), ".config")

    if (length(by) == 0L) {
      # Nothing to tune, just bind on config
      predictions <- vctrs::vec_cbind(predictions, .config)
    } else {
      predictions <- dplyr::inner_join(predictions, .config, by = by)
    }
  }

  dplyr::bind_rows(collection, predictions)
}

append_metrics <- function(
  workflow,
  collection,
  predictions,
  metrics,
  param_names,
  event_level,
  split,
  .config = NULL
) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  params <- predictions |>
    dplyr::select(dplyr::all_of(param_names)) |>
    dplyr::distinct()

  tmp_est <- metrics(workflow, new_data = rsample::analysis(split))

  tmp_est <- cbind(tmp_est, labels(split))

  tmp_est <- cbind(params, tmp_est)
  if (!rlang::is_null(.config)) {
    tmp_est <- cbind(tmp_est, .config)
  }
  dplyr::bind_rows(collection, tmp_est)
}

append_extracts <- function(
  collection,
  workflow,
  grid,
  split,
  ctrl,
  .config = NULL
) {
  extracts <-
    grid |>
    dplyr::bind_cols(labels(split)) |>
    dplyr::mutate(
      .extracts = list(
        extract_details(workflow, ctrl$extract)
      )
    )

  if (!rlang::is_null(.config)) {
    extracts <- cbind(extracts, .config)
  }

  dplyr::bind_rows(collection, extracts)
}

extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  try(extractor(object), silent = TRUE)
}
