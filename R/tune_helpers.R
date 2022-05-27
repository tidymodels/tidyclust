new_bare_tibble <- function(x, ..., class = character()) {
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
}

is_cataclysmic <- function(x) {
  is_err <- purrr::map_lgl(x$.metrics, inherits, c(
    "simpleError",
    "error"
  ))
  if (any(!is_err)) {
    is_good <- purrr::map_lgl(x$.metrics[!is_err], ~ tibble::is_tibble(.x) &&
      nrow(.x) > 0)
    is_err[!is_err] <- !is_good
  }
  all(is_err)
}

reduce_all_outcome_names <- function(resamples) {
  all_outcome_names <- resamples$.all_outcome_names
  all_outcome_names <- rlang::flatten(all_outcome_names)
  all_outcome_names <- vctrs::vec_unique(all_outcome_names)
  n_unique <- length(all_outcome_names)
  if (n_unique == 0L) {
    return(character())
  }
  if (n_unique > 1L) {
    rlang::warn(paste0(
      "More than one set of outcomes were used when tuning. ",
      "This should never happen. ",
      "Review how the outcome is specified in your model."
    ))
  }
  outcome_names <- all_outcome_names[[1L]]
  outcome_names
}

set_workflow <- function(workflow, control) {
  if (control$save_workflow) {
    if (!is.null(workflow$pre$actions$recipe)) {
      w_size <- utils::object.size(workflow$pre$actions$recipe)
      if (w_size / 1024^2 > 5) {
        msg <- paste0(
          "The workflow being saved contains a recipe, which is ",
          format(w_size, units = "Mb", digits = 2), " in memory. If this was not intentional, please set the control ",
          "setting `save_workflow = FALSE`."
        )
        cols <- get_celery_colors()
        msg <- strwrap(msg, prefix = paste0(
          cols$symbol$info(cli::symbol$info),
          " "
        ))
        msg <- cols$message$info(paste0(msg, collapse = "\n"))
        rlang::inform(msg)
      }
    }
    workflow
  } else {
    NULL
  }
}

new_tune_results <- function(x, parameters, metrics,
                             rset_info, ..., class = character()) {
  new_bare_tibble(
    x = x,
    parameters = parameters,
    metrics = metrics,
    rset_info = rset_info,
    ...,
    class = c(class, "tune_results")
  )
}

get_operator <- function(allow = TRUE, object) {
  is_par <- foreach::getDoParWorkers() > 1
  # pkgs <- required_pkgs(object) # TODO
  blacklist <- c("keras", "rJava")
  if (is_par & allow && any(pkgs %in% blacklist)) {
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

required_pkgs.cluster_spec <- function(x, infra = TRUE, ...) {
  if (is.null(x$engine)) {
    rlang::abort("Please set an engine.")
  }
  get_pkgs(x, infra)
}

required_pkgs.cluster_fit <- function(x, infra = TRUE, ...) {
  get_pkgs(x$spec, infra)
}

get_pkgs <- function(x, infra) {
  cls <- class(x)[1]
  pkgs <-
    get_from_env_celery(paste0(cls, "_pkgs")) %>%
    dplyr::filter(engine == x$engine)
  res <- pkgs$pkg[[1]]
  if (length(res) == 0) {
    res <- character(0)
  }
  if (infra) {
    infra_pkgs <- c("celery")
    res <- c(infra_pkgs, res)
  }
  res <- unique(res)
  res <- res[length(res) != 0]
  res
}

new_grid_info_resamples <- function() {
  msgs_preprocessor <- new_msgs_preprocessor(i = 1L, n = 1L)
  msgs_model <- new_msgs_model(i = 1L, n = 1L, msgs_preprocessor = msgs_preprocessor)
  iter_config <- list("Preprocessor1_Model1")
  out <- tibble::tibble(
    .iter_preprocessor = 1L, .msg_preprocessor = msgs_preprocessor,
    .iter_model = 1L, .iter_config = iter_config, .msg_model = msgs_model,
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

tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

#' @export
min_grid.cluster_spec <- function(x, grid, ...) {
  blank_submodels(grid)
}

blank_submodels <- function(grid) {
  grid %>%
    dplyr::mutate(
      .submodels = purrr::map(1:nrow(grid), ~ list())
    ) %>%
    dplyr::mutate_if(is.factor, as.character)
}

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

finalize_workflow_preprocessor <- function(workflow, grid_preprocessor) {
  if (ncol(grid_preprocessor) == 0L) {
    return(workflow)
  }
  recipe <- extract_preprocessor(workflow)
  recipe <- merge(recipe, grid_preprocessor)$x[[1]]
  workflow <- set_workflow_recipe(workflow, recipe)
  workflow
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
  res <- try(withCallingHandlers(warning = add_cond, expr),
    silent = TRUE
  )
  list(res = res, signals = signals)
}

siren <- function(x, type = "info") {
  celery_color <- get_celery_colors()
  types <- names(celery_color$message)
  type <- match.arg(type, types)
  msg <- glue::glue(x)
  symb <- dplyr::case_when(
    type == "warning" ~ celery_color$symbol$warning("!"),
    type == "go" ~ celery_color$symbol$go(cli::symbol$pointer),
    type == "danger" ~ celery_color$symbol$danger("x"), type ==
      "success" ~ celery_color$symbol$success(celery_symbol$success),
    type == "info" ~ celery_color$symbol$info("i")
  )
  msg <- dplyr::case_when(
    type == "warning" ~ celery_color$message$warning(msg),
    type == "go" ~ celery_color$message$go(msg), type == "danger" ~
      celery_color$message$danger(msg), type == "success" ~
      celery_color$message$success(msg), type == "info" ~
      celery_color$message$info(msg)
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
    wrn_msg <- purrr::map_chr(wrn, ~ .x$message)
    wrn_msg <- unique(wrn_msg)
    wrn_msg <- paste(wrn_msg, collapse = ", ")
    wrn_msg <- tibble::tibble(
      location = loc, type = "warning",
      note = wrn_msg
    )
    notes <- dplyr::bind_rows(notes, wrn_msg)
    wrn_msg <- glue::glue_collapse(paste0(loc, ": ", wrn_msg$note),
      width = options()$width - 5
    )
    tune_log(control2, split, wrn_msg, type = "warning")
  }
  if (inherits(res$res, "try-error")) {
    err_msg <- as.character(attr(res$res, "condition"))
    err_msg <- gsub("\n$", "", err_msg)
    err_msg <- tibble::tibble(
      location = loc, type = "error",
      note = err_msg
    )
    notes <- dplyr::bind_rows(notes, err_msg)
    err_msg <- glue::glue_collapse(paste0(loc, ": ", err_msg$note),
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

finalize_workflow_spec <- function(workflow, grid_model) {
  if (ncol(grid_model) == 0L) {
    return(workflow)
  }
  spec <- extract_spec_parsnip(workflow)
  spec <- merge(spec, grid_model)$x[[1]]
  workflow <- set_workflow_spec(workflow, spec)
  workflow
}

merge.cluster_spec <- function(x, y, ...) {
  merger(x, y, ...)
}

merger <- function(x, y, ...) {
  if (!is.data.frame(y)) {
    rlang::abort("The second argument should be a data frame.")
  }
  pset <- hardhat::extract_parameter_set_dials(x)
  if (nrow(pset) == 0) {
    res <- tibble::tibble(x = purrr::map(1:nrow(y), ~x))
    return(res)
  }
  grid_name <- colnames(y)
  if (inherits(x, "recipe")) {
    updater <- update_recipe
    step_ids <- purrr::map_chr(x$steps, ~ .x$id)
  } else {
    updater <- update_model
    step_ids <- NULL
  }
  if (!any(grid_name %in% pset$id)) {
    res <- tibble::tibble(x = purrr::map(1:nrow(y), ~x))
    return(res)
  }
  y %>%
    dplyr::mutate(
      ..object = purrr::map(
        1:nrow(y),
        ~ updater(y[.x, ], x, pset, step_ids, grid_name)
      )
    ) %>%
    dplyr::select(x = ..object)
}

set_workflow_spec <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

update_model <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset %>% dplyr::filter(id == i & source == "cluster_spec")
    if (nrow(param_info) > 1) {
      # TODO figure this out and write a better message
      rlang::abort("There are too many things.")
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

update_recipe <- function(grid, object, pset, step_id, nms, ...) {
  for (i in nms) {
    param_info <- pset %>% dplyr::filter(id == i & source == "recipe")
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
    rlang::abort("Internal error: Model result is not a workflow!")
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

predict_model <- function(split, workflow, grid, metrics, submodels = NULL) {
  model <- extract_fit_parsnip(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors
  y_vals <- forged$outcomes

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

    rlang::abort(msg)
  }

  # Determine the type of prediction that is required
  # type_info <- metrics_info(metrics)
  # types <- unique(type_info$type)
  types <- "cluster"

  res <- NULL
  merge_vars <- c(".row", names(grid))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      stats::predict(model, x_vals, type = type_iter) %>%
      dplyr::mutate(.row = orig_rows) %>%
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
      #     eval_tidy(mp_call) %>%
      #     mutate(.row = orig_rows) %>%
      #     unnest(cols = dplyr::starts_with(".pred")) %>%
      #     cbind(dplyr::select(grid, -dplyr::all_of(submod_param)), row.names = NULL) %>%
      #     # go back to user-defined name
      #     dplyr::rename(!!!make_rename_arg(grid, model, submodels)) %>%
      #     dplyr::select(dplyr::one_of(names(tmp_res))) %>%
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

  # Add outcome data
  # y_vals <- dplyr::mutate(y_vals, .row = orig_rows)
  # res <- dplyr::full_join(res, y_vals, by = ".row")

  tibble::as_tibble(res)
}

forge_from_workflow <- function(split, workflow) {
  new_data <- rsample::assessment(split)

  blueprint <- workflow$pre$mold$blueprint
  if (!rlang::is_installed("hardhat")) {
    rlang::abort(
      "Internal error: hardhat should have been installed from the workflows dependency."
    )
  }
  forged <- hardhat::forge(new_data, blueprint, outcomes = TRUE)

  forged
}

# ------------------------------------------------------------------------------

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

append_predictions <- function(collection,
                               predictions,
                               split,
                               control,
                               .config = NULL) {
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

append_metrics <- function(workflow,
                           collection,
                           predictions,
                           metrics,
                           param_names,
                           event_level,
                           split,
                           .config = NULL) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  params <- predictions %>%
    dplyr::select(dplyr::all_of(param_names)) %>%
    dplyr::distinct()

  tmp_est <- purrr::imap_dfr(metrics,
    ~ list(.estimate = .x(workflow)),
    .id = ".metric"
  ) %>%
    dplyr::mutate(.estimator = "standard")

  tmp_est <- cbind(tmp_est, labels(split))

  tmp_est <- cbind(params, tmp_est)
  if (!rlang::is_null(.config)) {
    tmp_est <- cbind(tmp_est, .config)
  }
  dplyr::bind_rows(collection, tmp_est)
}

append_extracts <- function(collection,
                            workflow,
                            grid,
                            split,
                            ctrl,
                            .config = NULL) {
  extracts <-
    grid %>%
    dplyr::bind_cols(labels(split)) %>%
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
  metrics <- purrr::map(results, ~ .x[[".metrics"]])
  extracts <- purrr::map(results, ~ .x[[".extracts"]])
  predictions <- purrr::map(results, ~ .x[[".predictions"]])
  all_outcome_names <- purrr::map(results, ~ .x[[".all_outcome_names"]])
  notes <- purrr::map(results, ~ .x[[".notes"]])
  metrics <- vctrs::vec_c(!!!metrics)
  extracts <- vctrs::vec_c(!!!extracts)
  predictions <- vctrs::vec_c(!!!predictions)
  all_outcome_names <- vctrs::vec_c(!!!all_outcome_names)
  notes <- vctrs::vec_c(!!!notes)
  list(
    .metrics = metrics, .extracts = extracts, .predictions = predictions,
    .all_outcome_names = all_outcome_names, .notes = notes
  )
}
