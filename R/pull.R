pull_metrics <- function(resamples, res, control) {
  out <- pulley(resamples, res, ".metrics")
  out$.metrics <- maybe_repair(out$.metrics)
  out
}

pull_notes <- function(resamples, res, control) {
  resamples$.notes <- map(res, `[[`, ".notes")
  resamples
}

pull_extracts <- function(resamples, res, control) {
  if (!is.null(control$extract)) {
    resamples <- pulley(resamples, res, ".extracts")
  }
  resamples
}

pull_predictions <- function(resamples, res, control) {
  if (control$save_pred) {
    resamples <- pulley(resamples, res, ".predictions")
    resamples$.predictions <- maybe_repair(resamples$.predictions)
  }
  resamples
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(resamples, res, col) {
  if (all(map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples |>
      dplyr::mutate(col = map(splits, \(x) NULL)) |>
      stats::setNames(c(names(resamples), col))
    return(res)
  }

  id_cols <- grep("^id", names(resamples), value = TRUE)
  resamples <- dplyr::arrange(resamples, !!!rlang::syms(id_cols))
  pulled_vals <- dplyr::bind_rows(map(res, \(x) x[[col]]))

  if (nrow(pulled_vals) == 0) {
    res <-
      resamples |>
      dplyr::mutate(col = map(splits, \(x) NULL)) |>
      stats::setNames(c(names(resamples), col))
    return(res)
  }

  pulled_vals <- tidyr::nest(pulled_vals, data = -dplyr::starts_with("id"))
  names(pulled_vals)[ncol(pulled_vals)] <- col

  res <- new_bare_tibble(resamples)
  res <- dplyr::full_join(res, pulled_vals, by = id_cols)
  res <- reup_rs(resamples, res)
  res
}

maybe_repair <- function(x) {
  not_null <- !map_lgl(x, is.null)
  is_tibb <- map_lgl(x, tibble::is_tibble)
  ok <- not_null & is_tibb
  if (!any(ok)) {
    return(x)
  }

  good_val <- which(ok)[1]
  template <- x[[good_val]][0, ]

  insert_val <- function(x, y) {
    if (is.null(x)) {
      x <- y
    }
    x
  }

  x <- map(x, insert_val, y = template)
  x
}
