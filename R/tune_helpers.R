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
      \(.x)
        tibble::is_tibble(.x) &&
          nrow(.x) > 0
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

# https://github.com/tidymodels/tune/blob/main/R/parallel.R
get_operator <- function(allow = TRUE, object) {
  is_par <- foreach::getDoParWorkers() > 1
  pkgs <- required_pkgs(object)
  blacklist <- c("keras", "rJava")
  if (is_par && allow && any(pkgs %in% blacklist)) {
    pkgs <- pkgs[pkgs %in% blacklist]
    msg <- paste0("'", pkgs, "'", collapse = ", ")
    msg <- paste(
      "Some required packages prohibit parallel processing: ",
      msg
    )
    cli::cli_alert_warning(msg)
    allow <- FALSE
  }
  cond <- allow && is_par
  if (cond) {
    res <- foreach::`%dopar%`
  } else {
    res <- foreach::`%do%`
  }
  res
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R
new_grid_info_resamples <- function() {
  msgs_preprocessor <- new_msgs_preprocessor(i = 1L, n = 1L)
  msgs_model <- new_msgs_model(
    i = 1L,
    n = 1L,
    msgs_preprocessor = msgs_preprocessor
  )
  iter_config <- list("Preprocessor1_Model1")
  out <- tibble::tibble(
    .iter_preprocessor = 1L,
    .msg_preprocessor = msgs_preprocessor,
    .iter_model = 1L,
    .iter_config = iter_config,
    .msg_model = msgs_model,
    .submodels = list(list())
  )
  out
}

new_msgs_preprocessor <- function(i, n) {
  paste0("preprocessor ", i, "/", n)
}

new_msgs_model <- function(i, n, msgs_preprocessor) {
  paste0(msgs_preprocessor, ", model ", i, "/", n)
}

# https://github.com/tidymodels/tune/blob/main/R/grid_code_paths.R
parallel_over_finalize <- function(parallel_over, n_resamples) {
  if (!is.null(parallel_over)) {
    return(parallel_over)
  }
  if (n_resamples == 1L) {
    "everything"
  } else {
    "resamples"
  }
}

# https://github.com/tidymodels/tune/blob/main/R/grid_code_paths.R
generate_seeds <- function(rng, n) {
  out <- vector("list", length = n)
  if (!rng) {
    return(out)
  }
  original_algorithms <- RNGkind(kind = "L'Ecuyer-CMRG")
  original_rng_algorithm <- original_algorithms[[1]]
  on.exit(RNGkind(kind = original_rng_algorithm), add = TRUE)
  seed <- .Random.seed
  for (i in seq_len(n)) {
    out[[i]] <- seed
    seed <- parallel::nextRNGStream(seed)
  }
  out
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

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R
compute_config_ids <- function(data, id_preprocessor) {
  submodels <- tidyr::unnest(data, .submodels, keep_empty = TRUE)
  submodels <- dplyr::pull(submodels, .submodels)
  model_sizes <- lengths(submodels) + 1L
  n_total_models <- sum(model_sizes)
  ids <- format_with_padding(seq_len(n_total_models))
  ids <- paste0(id_preprocessor, "_Model", ids)
  n_fit_models <- nrow(data)
  out <- vector("list", length = n_fit_models)
  start <- 1L
  for (i in seq_len(n_fit_models)) {
    size <- model_sizes[[i]]
    stop <- start + size - 1L
    out[[i]] <- ids[rlang::seq2(start, stop)]
    start <- stop + 1L
  }
  out
}

format_with_padding <- function(x) {
  gsub(" ", "0", format(x))
}

set_workflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}

catch_and_log <- function(.expr, ..., bad_only = FALSE, notes) {
  tune_log(..., type = "info")
  tmp <- catcher(.expr)
  new_notes <- log_problems(notes, ..., tmp, bad_only = bad_only)
  assign("out_notes", new_notes, envir = parent.frame())
  tmp$res
}

tune_log <- function(control, split = NULL, task, type = "success") {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  if (!is.null(split)) {
    labs <- labels(split)
    labs <- rev(unlist(labs))
    labs <- paste0(labs, collapse = ", ")
    labs <- paste0(labs, ": ")
  } else {
    labs <- ""
  }
  task <- gsub("\\{", "", task)
  siren(paste0(labs, task), type = type)
  NULL
}

catcher <- function(expr) {
  signals <- list()
  add_cond <- function(cnd) {
    signals <<- append(signals, list(cnd))
    rlang::cnd_muffle(cnd)
  }
  res <- try(withCallingHandlers(warning = add_cond, expr), silent = TRUE)
  list(res = res, signals = signals)
}

siren <- function(x, type = "info") {
  tidyclust_color <- get_tidyclust_colors()
  types <- names(tidyclust_color$message)
  type <- match.arg(type, types)
  msg <- glue::glue(x)
  symb <- dplyr::case_when(
    type == "warning" ~ tidyclust_color$symbol$warning("!"),
    type == "go" ~ tidyclust_color$symbol$go(cli::symbol$pointer),
    type == "danger" ~ tidyclust_color$symbol$danger("x"),
    type == "success" ~
      tidyclust_color$symbol$success(tidyclust_symbol$success),
    type == "info" ~ tidyclust_color$symbol$info("i")
  )
  msg <- dplyr::case_when(
    type == "warning" ~ tidyclust_color$message$warning(msg),
    type == "go" ~ tidyclust_color$message$go(msg),
    type == "danger" ~ tidyclust_color$message$danger(msg),
    type == "success" ~ tidyclust_color$message$success(msg),
    type == "info" ~ tidyclust_color$message$info(msg)
  )
  if (inherits(msg, "character")) {
    msg <- as.character(msg)
  }
  message(paste(symb, msg))
}

log_problems <- function(notes, control, split, loc, res, bad_only = FALSE) {
  control2 <- control
  control2$verbose <- TRUE
  wrn <- res$signals
  if (length(wrn) > 0) {
    wrn_msg <- map_chr(wrn, \(x) x$message)
    wrn_msg <- unique(wrn_msg)
    wrn_msg <- paste(wrn_msg, collapse = ", ")
    wrn_msg <- tibble::tibble(
      location = loc,
      type = "warning",
      note = wrn_msg
    )
    notes <- dplyr::bind_rows(notes, wrn_msg)
    wrn_msg <- glue::glue_collapse(
      paste0(loc, ": ", wrn_msg$note),
      width = options()$width - 5
    )
    tune_log(control2, split, wrn_msg, type = "warning")
  }
  if (inherits(res$res, "try-error")) {
    err_msg <- as.character(attr(res$res, "condition"))
    err_msg <- gsub("\n$", "", err_msg)
    err_msg <- tibble::tibble(
      location = loc,
      type = "error",
      note = err_msg
    )
    notes <- dplyr::bind_rows(notes, err_msg)
    err_msg <- glue::glue_collapse(
      paste0(loc, ": ", err_msg$note),
      width = options()$width - 5
    )
    tune_log(control2, split, err_msg, type = "danger")
  } else {
    if (!bad_only) {
      tune_log(control, split, loc, type = "success")
    }
  }
  notes
}

is_failure <- function(x) {
  inherits(x, "try-error")
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R
finalize_workflow_spec <- function(workflow, grid_model) {
  if (ncol(grid_model) == 0L) {
    return(workflow)
  }
  spec <- extract_spec_parsnip(workflow)
  spec <- merge(spec, grid_model)$x[[1]]
  workflow <- set_workflow_spec(workflow, spec)
  workflow
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

catch_and_log_fit <- function(expr, ..., notes) {
  tune_log(..., type = "info")
  caught <- catcher(expr)
  result <- caught$res
  if (is_failure(result)) {
    result_parsnip <- list(res = result, signals = list())
    new_notes <- log_problems(notes, ..., result_parsnip)
    assign("out_notes", new_notes, envir = parent.frame())
    return(result)
  }
  if (!is_workflow(result)) {
    cli::cli_abort("Internal error: Model result is not a workflow!")
  }
  fit <- result$fit$fit$fit
  if (is_failure(fit)) {
    result_fit <- list(res = fit, signals = list())
    new_notes <- log_problems(notes, ..., result_fit)
    assign("out_notes", new_notes, envir = parent.frame())
    return(result)
  }
  new_notes <- log_problems(notes, ..., caught)
  assign("out_notes", new_notes, envir = parent.frame())
  result
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R
predict_model <- function(split, workflow, grid, metrics, submodels = NULL) {
  model <- extract_fit_parsnip(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors

  orig_rows <- as.integer(split, data = "assessment")

  if (length(orig_rows) != nrow(x_vals)) {
    msg <- paste0(
      "Some assessment set rows are not available at ",
      "prediction time. "
    )

    if (has_preprocessor_recipe(workflow)) {
      msg <- paste0(
        msg,
        "Consider using `skip = TRUE` on any recipe steps that remove rows ",
        "to avoid calling them on the assessment set."
      )
    } else {
      msg <- paste0(
        msg,
        "Did your preprocessing steps filter or remove rows?"
      )
    }

    cli::cli_abort(msg)
  }

  # Determine the type of prediction that is required
  types <- "cluster"

  res <- NULL
  merge_vars <- c(".row", names(grid))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      stats::predict(model, x_vals, type = type_iter) |>
      dplyr::mutate(.row = orig_rows) |>
      cbind(grid, row.names = NULL)

    if (!is.null(submodels)) {
      submod_length <- lengths(submodels)
      has_submodels <- any(submod_length > 0)

      # if (has_submodels) {
      #   submod_param <- names(submodels)
      #   mp_call <-
      #     call2(
      #       "multi_predict",
      #       .ns = "parsnip",
      #       object = expr(model),
      #       new_data = expr(x_vals),
      #       type = type_iter,
      #       !!!make_submod_arg(grid, model, submodels)
      #     )
      #   tmp_res <-
      #     eval_tidy(mp_call) |>
      #     mutate(.row = orig_rows) |>
      #     unnest(cols = dplyr::starts_with(".pred")) |>
      #     cbind(dplyr::select(grid, -dplyr::all_of(submod_param)),
      #           row.names = NULL) |>
      #     # go back to user-defined name
      #     dplyr::rename(!!!make_rename_arg(grid, model, submodels)) |>
      #     dplyr::select(dplyr::one_of(names(tmp_res))) |>
      #     dplyr::bind_rows(tmp_res)
      # }
    }

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  } # end type loop

  tibble::as_tibble(res)
}

forge_from_workflow <- function(split, workflow) {
  new_data <- rsample::assessment(split)

  blueprint <- workflow$pre$mold$blueprint

  # Can't use tune version since outcomes = FALSE
  forged <- hardhat::forge(new_data, blueprint, outcomes = FALSE)

  forged
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

# ------------------------------------------------------------------------------

# https://github.com/tidymodels/tune/blob/main/R/pull.R#L210
extract_metrics_config <- function(param_names, metrics) {
  metrics_config_names <- c(param_names, ".config")
  out <- metrics[metrics_config_names]
  vctrs::vec_unique(out)
}

# https://github.com/tidymodels/tune/blob/main/R/tune_bayes.R#L784
# Make sure that rset object attributes are kept once joined
reup_rs <- function(resamples, res) {
  sort_cols <- grep("^id", names(resamples), value = TRUE)
  if (any(names(res) == ".iter")) {
    sort_cols <- c(".iter", sort_cols)
  }
  res <- dplyr::arrange(res, !!!rlang::syms(sort_cols))
  att <- attributes(res)
  rsample_att <- attributes(resamples)
  for (i in names(rsample_att)) {
    if (!any(names(att) == i)) {
      attr(res, i) <- rsample_att[[i]]
    }
  }

  class(res) <- unique(c("tune_results", class(res)))
  res
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}

grid_msg <- "`grid` should be a positive integer or a data frame."

slice_seeds <- function(x, i, n) {
  x[(i - 1L) * n + seq_len(n)]
}

iter_combine <- function(...) {
  results <- list(...)
  metrics <- map(results, \(.x) .x[[".metrics"]])
  extracts <- map(results, \(.x) .x[[".extracts"]])
  predictions <- map(results, \(.x) .x[[".predictions"]])
  notes <- map(results, \(.x) .x[[".notes"]])
  metrics <- vctrs::vec_c(!!!metrics)
  extracts <- vctrs::vec_c(!!!extracts)
  predictions <- vctrs::vec_c(!!!predictions)
  notes <- vctrs::vec_c(!!!notes)
  list(
    .metrics = metrics,
    .extracts = extracts,
    .predictions = predictions,
    .notes = notes
  )
}
