# https://github.com/tidymodels/tune/blob/main/R/tune_grid.R

#' Model tuning via grid search
#'
#' [tune_cluster()] computes a set of performance metrics (e.g. accuracy or
#' RMSE) for a pre-defined set of tuning parameters that correspond to a model
#' or recipe across one or more resamples of the data.
#'
#' @param object A `tidyclust` model specification or a [workflows::workflow()].
#' @param preprocessor A traditional model formula or a recipe created using
#'   [recipes::recipe()].
#' @param resamples An `rset()` object.
#' @param param_info A [dials::parameters()] object or `NULL`. If none is given,
#'   a parameters set is derived from other arguments. Passing this argument can
#'   be useful when parameter ranges need to be customized.
#' @param grid A data frame of tuning combinations or a positive integer. The
#'   data frame should have columns for each parameter being tuned and rows for
#'   tuning parameter candidates. An integer denotes the number of candidate
#'   parameter sets to be created automatically.
#' @param metrics A [cluster_metric_set()] or `NULL`.
#' @param control An object used to modify the tuning process. Defaults to
#'   `tune::control_grid()`.
#' @param ... Not currently used.
#' @return An updated version of `resamples` with extra list columns for
#'   `.metrics` and `.notes` (optional columns are `.predictions` and
#'   `.extracts`). `.notes` contains warnings and errors that occur during
#'   execution.
#'
#' @examples
#' library(recipes)
#' library(rsample)
#' library(workflows)
#' library(tune)
#'
#' rec_spec <- recipe(~., data = mtcars) |>
#'   step_normalize(all_numeric_predictors()) |>
#'   step_pca(all_numeric_predictors())
#'
#' kmeans_spec <- k_means(num_clusters = tune())
#'
#' wflow <- workflow() |>
#'   add_recipe(rec_spec) |>
#'   add_model(kmeans_spec)
#'
#' grid <- tibble(num_clusters = 1:3)
#'
#' set.seed(4400)
#' folds <- vfold_cv(mtcars, v = 2)
#'
#' res <- tune_cluster(
#'   wflow,
#'   resamples = folds,
#'   grid = grid
#' )
#' res
#'
#' collect_metrics(res)
#' @export
tune_cluster <- function(object, ...) {
  UseMethod("tune_cluster")
}

#' @export
tune_cluster.default <- function(object, ...) {
  cli::cli_abort(
    "The first argument to {.fn tune_cluster} should be either a model or workflow."
  )
}

#' @export
#' @rdname tune_cluster
tune_cluster.cluster_spec <- function(
  object,
  preprocessor,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = tune::control_grid()
) {
  if (rlang::is_missing(preprocessor) || !tune::is_preprocessor(preprocessor)) {
    cli::cli_abort(
      "To tune a model spec, you must preprocess with a formula or recipe."
    )
  }

  tune::empty_ellipses(...)

  control <- parsnip::condense_control(control, tune::control_grid())

  wflow <- workflows::add_model(workflows::workflow(), object)

  if (tune::is_recipe(preprocessor)) {
    wflow <- workflows::add_recipe(wflow, preprocessor)
  } else if (rlang::is_formula(preprocessor)) {
    wflow <- workflows::add_formula(wflow, preprocessor)
  }

  tune_cluster(
    wflow,
    resamples = resamples,
    param_info = param_info,
    grid = grid,
    metrics = metrics,
    control = control
  )
}

#' @export
#' @rdname tune_cluster
tune_cluster.workflow <- function(
  object,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = tune::control_grid()
) {
  tune::empty_ellipses(...)

  control <- parsnip::condense_control(control, tune::control_grid())

  # Disallow `NULL` grids in `tune_cluster()`, as this is the special signal
  # used when no tuning is required
  if (is.null(grid)) {
    cli::cli_abort(grid_msg)
  }

  tune_cluster_workflow(
    object,
    resamples = resamples,
    grid = grid,
    metrics = metrics,
    pset = param_info,
    control = control
  )
}

# ------------------------------------------------------------------------------

tune_cluster_workflow <- function(
  workflow,
  resamples,
  grid = 10,
  metrics = NULL,
  pset = NULL,
  control = NULL,
  rng = TRUE
) {
  tune::check_rset(resamples)

  metrics <- check_metrics(metrics, workflow)

  pset <- check_parameters(
    workflow = workflow,
    pset = pset,
    data = resamples$splits[[1]]$data,
    grid_names = names(grid)
  )

  check_workflow(workflow, pset = pset)

  grid <- check_grid(
    grid = grid,
    workflow = workflow,
    pset = pset
  )

  # Save rset attributes, then fall back to a bare tibble
  rset_info <- tune::pull_rset_attributes(resamples)
  resamples <- new_bare_tibble(resamples)

  resamples <- tune_cluster_loop(
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    metrics = metrics,
    control = control,
    rng = rng
  )

  if (is_cataclysmic(resamples)) {
    cli::cli_warn(
      c(
        "All models failed.",
        "i" = "See the {.code .notes} column."
      )
    )
  }

  workflow <- set_workflow(workflow, control)

  new_tune_results(
    x = resamples,
    parameters = pset,
    metrics = metrics,
    rset_info = rset_info,
    workflow = workflow
  )
}

tune_cluster_loop <- function(
  resamples,
  grid,
  workflow,
  metrics,
  control,
  rng
) {
  `%op%` <- get_operator(control$allow_par, workflow)
  `%:%` <- foreach::`%:%`

  packages <- c(control$pkgs, required_pkgs(workflow))

  grid_info <- compute_grid_info(workflow, grid)

  n_resamples <- nrow(resamples)
  iterations <- seq_len(n_resamples)

  n_grid_info <- nrow(grid_info)
  rows <- seq_len(n_grid_info)

  splits <- resamples$splits

  parallel_over <- control$parallel_over
  parallel_over <- parallel_over_finalize(parallel_over, n_resamples)

  rlang::local_options(doFuture.rng.onMisuse = "ignore")

  if (identical(parallel_over, "resamples")) {
    seeds <- generate_seeds(rng, n_resamples)

    # We are wrapping in `local()` here because `fn_tune_grid_loop_iter_safely()` adds
    # on.exit/deferred handlers to our execution frame by passing `tune_env$progress_env`
    # to cli's progress bar constructor, which then adds an exit handler on that
    # environment. Because `%op%` evaluates its expression in `eval()` in the calling
    # environment (i.e. here), the handlers are added in the special frame environment
    # created by `eval()`. This causes the handler to run much too early. By evaluating in
    # a local environment, we prevent `defer()`/`on.exit()` from finding the short-lived
    # context of `%op%`. Instead it looks all the way up here to register the handler.

    results <- local({
      suppressPackageStartupMessages(
        foreach::foreach(
          split = splits,
          seed = seeds,
          .packages = packages,
          .errorhandling = "pass"
        ) %op%
          {
            # Extract internal function from tune namespace
            tune_cluster_loop_iter_safely <- utils::getFromNamespace(
              x = "tune_cluster_loop_iter_safely",
              ns = "tidyclust"
            )

            tune_cluster_loop_iter_safely(
              split = split,
              grid_info = grid_info,
              workflow = workflow,
              metrics = metrics,
              control = control,
              seed = seed
            )
          }
      )
    })
  } else if (identical(parallel_over, "everything")) {
    seeds <- generate_seeds(rng, n_resamples * n_grid_info)

    results <- local(
      suppressPackageStartupMessages(
        foreach::foreach(
          iteration = iterations,
          split = splits,
          .packages = packages,
          .errorhandling = "pass"
        ) %:%
          foreach::foreach(
            row = rows,
            seed = slice_seeds(seeds, iteration, n_grid_info),
            .packages = packages,
            .errorhandling = "pass",
            .combine = iter_combine
          ) %op%
          {
            # Extract internal function from tidyclust namespace
            tune_grid_loop_iter_safely <- utils::getFromNamespace(
              x = "tune_cluster_loop_iter_safely",
              ns = "tidyclust"
            )

            grid_info_row <- vctrs::vec_slice(grid_info, row)

            tune_grid_loop_iter_safely(
              split = split,
              grid_info = grid_info_row,
              workflow = workflow,
              metrics = metrics,
              control = control,
              seed = seed
            )
          }
      )
    )
  } else {
    cli::cli_abort("Internal error: Invalid {.arg parallel_over}.")
  }

  resamples <- pull_metrics(resamples, results, control)
  resamples <- pull_notes(resamples, results, control)
  resamples <- pull_extracts(resamples, results, control)
  resamples <- pull_predictions(resamples, results, control)

  resamples
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R#L299
compute_grid_info <- function(workflow, grid) {
  if (is.null(grid)) {
    out <- new_grid_info_resamples()
    return(out)
  }
  grid <- tibble::as_tibble(grid)
  parameters <- hardhat::extract_parameter_set_dials(workflow)
  parameters_model <- dplyr::filter(parameters, source == "cluster_spec")
  parameters_preprocessor <- dplyr::filter(parameters, source == "recipe")
  any_parameters_model <- nrow(parameters_model) > 0
  any_parameters_preprocessor <- nrow(parameters_preprocessor) > 0
  if (any_parameters_model) {
    if (any_parameters_preprocessor) {
      compute_grid_info_model_and_preprocessor(
        workflow,
        grid,
        parameters_model
      )
    } else {
      compute_grid_info_model(workflow, grid, parameters_model)
    }
  } else {
    if (any_parameters_preprocessor) {
      compute_grid_info_preprocessor(workflow, grid, parameters_model)
    } else {
      cli::cli_abort(
        c(
          "Internal error: {.code workflow} should have some tunable parameters
     if {.code grid} is not {.code NULL}."
        )
      )
    }
  }
}

tune_cluster_loop_iter <- function(
  split,
  grid_info,
  workflow,
  metrics,
  control,
  seed
) {
  load_pkgs(workflow)
  load_namespace(control$pkgs)

  # After package loading to avoid potential package RNG manipulation
  if (!is.null(seed)) {
    # `assign()`-ing the random seed alters the `kind` type to L'Ecuyer-CMRG,
    # so we have to ensure it is restored on exit
    old_kind <- RNGkind()[[1]]
    assign(".Random.seed", seed, envir = globalenv())
    on.exit(RNGkind(kind = old_kind), add = TRUE)
  }

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- workflows::control_workflow(control_parsnip)

  event_level <- control$event_level

  out_metrics <- NULL
  out_extracts <- NULL
  out_predictions <- NULL
  out_notes <- tibble::tibble(
    location = character(0),
    type = character(0),
    note = character(0)
  )

  params <- hardhat::extract_parameter_set_dials(workflow)
  model_params <- dplyr::filter(params, source == "cluster_spec")
  preprocessor_params <- dplyr::filter(params, source == "recipe")

  param_names <- dplyr::pull(params, "id")
  model_param_names <- dplyr::pull(model_params, "id")
  preprocessor_param_names <- dplyr::pull(preprocessor_params, "id")

  # Model related grid-info columns
  cols <- rlang::expr(
    c(
      .iter_model,
      .iter_config,
      .msg_model,
      dplyr::all_of(model_param_names),
      .submodels
    )
  )

  # Nest grid_info:
  # - Preprocessor info in the outer level
  # - Model info in the inner level
  grid_info <- tidyr::nest(grid_info, data = !!cols)

  training <- rsample::analysis(split)

  # ----------------------------------------------------------------------------
  # Preprocessor loop

  iter_preprocessors <- grid_info[[".iter_preprocessor"]]

  workflow_original <- workflow

  for (iter_preprocessor in iter_preprocessors) {
    workflow <- workflow_original

    iter_grid_info <- dplyr::filter(
      .data = grid_info,
      .iter_preprocessor == iter_preprocessor
    )

    iter_grid_preprocessor <- dplyr::select(
      .data = iter_grid_info,
      dplyr::all_of(preprocessor_param_names)
    )

    iter_msg_preprocessor <- iter_grid_info[[".msg_preprocessor"]]

    workflow <- tune::finalize_workflow_preprocessor(
      workflow = workflow,
      grid_preprocessor = iter_grid_preprocessor
    )

    workflow <- catch_and_log(
      .expr = workflows::.fit_pre(workflow, training),
      control,
      split,
      iter_msg_preprocessor,
      notes = out_notes
    )

    if (is_failure(workflow)) {
      next
    }

    # --------------------------------------------------------------------------
    # Model loop

    iter_grid_info_models <- iter_grid_info[["data"]][[1L]]
    iter_models <- iter_grid_info_models[[".iter_model"]]

    workflow_preprocessed <- workflow

    for (iter_model in iter_models) {
      workflow <- workflow_preprocessed

      iter_grid_info_model <- dplyr::filter(
        .data = iter_grid_info_models,
        .iter_model == iter_model
      )

      iter_grid_model <- dplyr::select(
        .data = iter_grid_info_model,
        dplyr::all_of(model_param_names)
      )

      iter_submodels <- iter_grid_info_model[[".submodels"]][[1L]]
      iter_msg_model <- iter_grid_info_model[[".msg_model"]]
      iter_config <- iter_grid_info_model[[".iter_config"]][[1L]]

      workflow <- finalize_workflow_spec(workflow, iter_grid_model)

      workflow <- catch_and_log_fit(
        expr = workflows::.fit_model(workflow, control_workflow),
        control,
        split,
        iter_msg_model,
        notes = out_notes
      )

      # Check for parsnip level and model level failure
      if (is_failure(workflow) || is_failure(workflow$fit$fit$fit)) {
        next
      }

      workflow <- workflows::.fit_finalize(workflow)

      # FIXME: I think this might be wrong? Doesn't use submodel parameters,
      # so `extracts` column doesn't list the correct parameters.
      iter_grid <- dplyr::bind_cols(
        iter_grid_preprocessor,
        iter_grid_model
      )

      # FIXME: bind_cols() drops number of rows with zero col data frames
      # because of a bug with vec_cbind()
      # https://github.com/r-lib/vctrs/issues/1281
      if (ncol(iter_grid_preprocessor) == 0L && ncol(iter_grid_model) == 0L) {
        nrow <- nrow(iter_grid_model)
        iter_grid <- tibble::new_tibble(x = list(), nrow = nrow)
      }

      out_extracts <- append_extracts(
        collection = out_extracts,
        workflow = workflow,
        grid = iter_grid,
        split = split,
        ctrl = control,
        .config = iter_config
      )

      iter_msg_predictions <- paste(iter_msg_model, "(predictions)")

      iter_predictions <- catch_and_log(
        predict_model(split, workflow, iter_grid, metrics, iter_submodels),
        control,
        split,
        iter_msg_predictions,
        bad_only = TRUE,
        notes = out_notes
      )

      # Check for prediction level failure
      if (is_failure(iter_predictions)) {
        next
      }

      out_metrics <- append_metrics(
        workflow = workflow,
        collection = out_metrics,
        predictions = iter_predictions,
        metrics = metrics,
        param_names = param_names,
        event_level = event_level,
        split = split,
        .config = iter_config
      )

      iter_config_metrics <- extract_metrics_config(param_names, out_metrics)

      out_predictions <- append_predictions(
        collection = out_predictions,
        predictions = iter_predictions,
        split = split,
        control = control,
        .config = iter_config_metrics
      )
    } # model loop
  } # preprocessor loop

  list(
    .metrics = out_metrics,
    .extracts = out_extracts,
    .predictions = out_predictions,
    .notes = out_notes
  )
}

tune_cluster_loop_iter_safely <- function(
  split,
  grid_info,
  workflow,
  metrics,
  control,
  seed
) {
  tune_cluster_loop_iter_wrapper <- super_safely(tune_cluster_loop_iter)

  time <- proc.time()
  result <- tune_cluster_loop_iter_wrapper(
    split,
    grid_info,
    workflow,
    metrics,
    control,
    seed
  )
  new_time <- proc.time()

  # Update with elapsed time
  result$result[[".elapsed"]] <- new_time["elapsed"] - time["elapsed"]

  error <- result$error
  warnings <- result$warnings
  result <- result$result

  # No problems
  if (is.null(error) && length(warnings) == 0L) {
    return(result)
  }

  # No errors, but we might have warning notes
  if (is.null(error)) {
    res <- result
    notes <- result$.notes
  } else {
    res <- error
    notes <- NULL
  }

  problems <- list(res = res, signals = warnings)

  notes <- log_problems(notes, control, split, "internal", problems)

  # Need an output template
  if (!is.null(error)) {
    result <- list(
      .metrics = NULL,
      .extracts = NULL,
      .predictions = NULL,
      .notes = NULL
    )
  }

  # Update with new notes
  result[[".notes"]] <- notes

  result
}

# https://github.com/tidymodels/tune/blob/main/R/grid_code_paths.R#L542
super_safely <- function(fn) {
  warnings <- list()
  handle_error <- function(e) {
    e <- structure(e$message, class = "try-error", condition = e)
    list(result = NULL, error = e, warnings = warnings)
  }
  handle_warning <- function(w) {
    warnings <<- c(warnings, list(w))
    rlang::cnd_muffle(w)
  }
  safe_fn <- function(...) {
    withCallingHandlers(
      expr = tryCatch(
        expr = list(
          result = fn(...),
          error = NULL,
          warnings = warnings
        ),
        error = handle_error
      ),
      warning = handle_warning
    )
  }
  safe_fn
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R#L428
compute_grid_info_model <- function(workflow, grid, parameters_model) {
  spec <- extract_spec_parsnip(workflow)
  out <- min_grid(spec, grid)
  n_fit_models <- nrow(out)
  seq_fit_models <- seq_len(n_fit_models)
  msgs_preprocessor <- new_msgs_preprocessor(i = 1L, n = 1L)
  msgs_preprocessor <- rep(msgs_preprocessor, times = n_fit_models)
  msgs_model <- new_msgs_model(
    i = seq_fit_models,
    n = n_fit_models,
    msgs_preprocessor = msgs_preprocessor
  )
  iter_configs <- compute_config_ids(out, "Preprocessor1")
  out <- tibble::add_column(
    .data = out,
    .iter_preprocessor = 1L,
    .before = 1L
  )
  out <- tibble::add_column(
    .data = out,
    .msg_preprocessor = msgs_preprocessor,
    .after = ".iter_preprocessor"
  )
  out <- tibble::add_column(
    .data = out,
    .iter_model = seq_fit_models,
    .after = ".msg_preprocessor"
  )
  out <- tibble::add_column(
    .data = out,
    .iter_config = iter_configs,
    .after = ".iter_model"
  )
  out <- tibble::add_column(
    .data = out,
    .msg_model = msgs_model,
    .after = ".iter_config"
  )
  out
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R#L484
compute_grid_info_model_and_preprocessor <- function(
  workflow,
  grid,
  parameters_model
) {
  parameter_names_model <- parameters_model[["id"]]

  # Nest model parameters, keep preprocessor parameters outside
  out <- tidyr::nest(grid, data = dplyr::all_of(parameter_names_model))

  n_preprocessors <- nrow(out)
  seq_preprocessors <- seq_len(n_preprocessors)

  # preprocessor <i_pre>/<n_pre>
  msgs_preprocessor <- new_msgs_preprocessor(
    i = seq_preprocessors,
    n = n_preprocessors
  )

  out <- tibble::add_column(
    .data = out,
    .iter_preprocessor = seq_preprocessors,
    .before = 1L
  )

  out <- tibble::add_column(
    .data = out,
    .msg_preprocessor = msgs_preprocessor,
    .after = ".iter_preprocessor"
  )

  spec <- extract_spec_parsnip(workflow)

  ids_preprocessor <- format_with_padding(seq_preprocessors)
  ids_preprocessor <- paste0("Preprocessor", ids_preprocessor)

  model_grids <- out[["data"]]

  for (i in seq_preprocessors) {
    model_grid <- model_grids[[i]]

    model_grid <- min_grid(spec, model_grid)

    n_fit_models <- nrow(model_grid)
    seq_fit_models <- seq_len(n_fit_models)

    msg_preprocessor <- msgs_preprocessor[[i]]
    id_preprocessor <- ids_preprocessor[[i]]

    # preprocessor <i_pre>/<n_pre>, model <i_mod>/<n_mod>
    msgs_model <- new_msgs_model(
      i = seq_fit_models,
      n = n_fit_models,
      msgs_preprocessor = msg_preprocessor
    )

    # Preprocessor<i_pre>_Model<i>
    iter_configs <- compute_config_ids(model_grid, id_preprocessor)

    model_grid <- tibble::add_column(
      .data = model_grid,
      .iter_model = seq_fit_models,
      .before = 1L
    )

    model_grid <- tibble::add_column(
      .data = model_grid,
      .iter_config = iter_configs,
      .after = ".iter_model"
    )

    model_grid <- tibble::add_column(
      .data = model_grid,
      .msg_model = msgs_model,
      .after = ".iter_config"
    )

    model_grids[[i]] <- model_grid
  }

  out[["data"]] <- model_grids

  # Unnest to match other grid-info generators
  out <- tidyr::unnest(out, data)

  out
}

# https://github.com/tidymodels/tune/blob/main/R/grid_helpers.R#L359
compute_grid_info_preprocessor <- function(workflow, grid, parameters_model) {
  out <- grid

  n_preprocessors <- nrow(out)
  seq_preprocessors <- seq_len(n_preprocessors)

  # Preprocessor<i>_Model1
  ids <- format_with_padding(seq_preprocessors)
  iter_configs <- paste0("Preprocessor", ids, "_Model1")
  iter_configs <- as.list(iter_configs)

  # preprocessor <i>/<n>
  msgs_preprocessor <- new_msgs_preprocessor(
    i = seq_preprocessors,
    n = n_preprocessors
  )

  # preprocessor <i>/<n>, model 1/1
  msgs_model <- new_msgs_model(
    i = 1L,
    n = 1L,
    msgs_preprocessor = msgs_preprocessor
  )

  # Manually add .submodels column, which will always have empty lists
  submodels <- rep_len(list(list()), n_preprocessors)

  out <- tibble::add_column(
    .data = out,
    .iter_preprocessor = seq_preprocessors,
    .before = 1L
  )

  out <- tibble::add_column(
    .data = out,
    .msg_preprocessor = msgs_preprocessor,
    .after = ".iter_preprocessor"
  )

  # Add at the end
  out <- tibble::add_column(
    .data = out,
    .iter_model = 1L,
    .after = NULL
  )

  out <- tibble::add_column(
    .data = out,
    .iter_config = iter_configs,
    .after = ".iter_model"
  )

  out <- tibble::add_column(
    .data = out,
    .msg_model = msgs_model,
    .after = ".iter_config"
  )

  out <- tibble::add_column(
    .data = out,
    .submodels = submodels,
    .after = ".msg_model"
  )

  out
}

# https://github.com/tidymodels/tune/blob/main/R/checks.R#L338
check_metrics <- function(x, object) {
  mode <- extract_spec_parsnip(object)$mode

  if (is.null(x)) {
    switch(
      mode,
      partition = {
        x <- cluster_metric_set(sse_within_total, sse_total)
      },
      unknown = {
        cli::cli_abort(
          "Internal error: {.fn check_installs} should have caught an {.code unknown} mode."
        )
      },
      cli::cli_abort("Unknown {.arg mode} for tidyclust model.")
    )

    return(x)
  }

  is_cluster_metric_set <- inherits(x, "cluster_metric_set")

  if (!is_cluster_metric_set) {
    cli::cli_abort(
      "The {.arg metrics} argument should be the results of {.fn cluster_metric_set}."
    )
  }
  x
}

# https://github.com/tidymodels/tune/blob/main/R/checks.R#L144
check_parameters <- function(
  workflow,
  pset = NULL,
  data,
  grid_names = character(0)
) {
  if (is.null(pset)) {
    pset <- hardhat::extract_parameter_set_dials(workflow)
  }
  unk <- map_lgl(pset$object, dials::has_unknowns)
  if (!any(unk)) {
    return(pset)
  }
  tune_param <- generics::tune_args(workflow)
  tune_recipe <- tune_param$id[tune_param$source == "recipe"]
  tune_recipe <- length(tune_recipe) > 0

  if (needs_finalization(pset, grid_names)) {
    if (tune_recipe) {
      cli::cli_abort(
        c(
          "Some tuning parameters require finalization but there are recipe 
          parameters that require tuning.",
          "i" = "Please use {.fn parameters} to finalize the parameter ranges."
        )
      )
    }
    msg <- "Creating pre-processing data to finalize unknown parameter"
    unk_names <- pset$id[unk]
    if (length(unk_names) == 1) {
      msg <- paste0(msg, ": ", unk_names)
    } else {
      msg <- paste0(msg, "s: ", paste0("'", unk_names, "'", collapse = ", "))
    }

    tune_log(list(verbose = TRUE), split = NULL, msg, type = "info")

    x <- workflows::.fit_pre(workflow, data)$pre$mold$predictors
    pset$object <- map(pset$object, dials::finalize, x = x)
  }
  pset
}

needs_finalization <- function(x, nms = character(0)) {
  # If an unknown engine-specific parameter, the object column is missing and
  # no need for finalization
  x <- x[!is.na(x$object), ]
  # If the parameter is in a pre-defined grid, then no need to finalize
  x <- x[!(x$id %in% nms), ]
  if (length(x) == 0) {
    return(FALSE)
  }
  any(dials::has_unknowns(x$object))
}

# https://github.com/tidymodels/tune/blob/main/R/checks.R#L274
check_workflow <- function(x, pset = NULL, check_dials = FALSE) {
  if (!inherits(x, "workflow")) {
    cli::cli_abort(
      "The {.arg object} argument should be a {.cls workflow} object."
    )
  }

  if (!has_preprocessor(x)) {
    cli::cli_abort("A formula, recipe, or variables preprocessor is required.")
  }

  if (!has_spec(x)) {
    cli::cli_abort("A tidyclust model is required.")
  }

  if (check_dials) {
    if (is.null(pset)) {
      pset <- hardhat::extract_parameter_set_dials(x)
    }

    check_param_objects(pset)

    incompl <- dials::has_unknowns(pset$object)

    if (any(incompl)) {
      cli::cli_abort(
        "The workflow has arguments whose ranges are not finalized: {.arg {pset$id[incompl]}}."
      )
    }
  }

  mod <- extract_spec_parsnip(x)
  check_installs(mod)

  invisible(NULL)
}

# https://github.com/tidymodels/tune/blob/main/R/checks.R#L257
check_param_objects <- function(pset) {
  params <- map_lgl(pset$object, inherits, "param")

  if (!all(params)) {
    cli::cli_abort(
      "The workflow has arguments to be tuned that are missing parameter objects: {.arg {pset$id[!params]}}."
    )
  }
  invisible(pset)
}

grid_msg <- "`grid` should be a positive integer or a data frame."

# https://github.com/tidymodels/tune/blob/main/R/checks.R#L36
check_grid <- function(grid, workflow, pset = NULL) {
  # `NULL` grid is the signal that we are using `fit_resamples()`
  if (is.null(grid)) {
    return(grid)
  }

  if (is.null(pset)) {
    pset <- hardhat::extract_parameter_set_dials(workflow)
  }

  if (nrow(pset) == 0L) {
    cli::cli_warn(
      c(
        "No tuning parameters have been detected, performance will be evaluated using 
        the resamples with no tuning.",
        "i" = "Did you want to {.fn tune} parameters?"
      )
    )

    # Return `NULL` as the new `grid`, like what is used in `fit_resamples()`
    return(NULL)
  }

  if (!is.numeric(grid)) {
    if (!is.data.frame(grid)) {
      cli::cli_abort(grid_msg)
    }

    grid_distinct <- dplyr::distinct(grid)
    if (!identical(nrow(grid_distinct), nrow(grid))) {
      cli::cli_warn(
        "Duplicate rows in grid of tuning combinations found and removed."
      )
    }
    grid <- grid_distinct

    tune_tbl <- generics::tune_args(workflow)
    tune_params <- tune_tbl$id

    # when called from [tune_bayes()]
    tune_params <- tune_params[tune_params != ".iter"]

    grid_params <- names(grid)

    extra_grid_params <- setdiff(grid_params, tune_params)
    extra_tune_params <- setdiff(tune_params, grid_params)

    if (length(extra_grid_params) != 0L) {
      extra_grid_params <- glue::single_quote(extra_grid_params)
      extra_grid_params <- glue::glue_collapse(extra_grid_params, sep = ", ")

      cli::cli_abort(
        "The provided {.arg grid} has parameter column{?s} {extra_grid_params} 
  that {?has/have} not been marked for tuning by {.fn tune}."
      )
    }

    if (length(extra_tune_params) != 0L) {
      extra_tune_params <- glue::single_quote(extra_tune_params)
      extra_tune_params <- glue::glue_collapse(extra_tune_params, sep = ", ")

      cli::cli_abort(
        "The provided {.arg grid} is missing parameter column{?s} {.val {extra_tune_params}}
   that {?has/have} been marked for tuning by {.fn tune}."
      )
    }
  } else {
    grid <- as.integer(grid[1])
    if (grid < 1) {
      cli::cli_abort(grid_msg)
    }
    check_workflow(workflow, pset = pset, check_dials = TRUE)

    grid <- dials::grid_space_filling(pset, size = grid)
    grid <- dplyr::distinct(grid)
  }

  if (!tibble::is_tibble(grid)) {
    grid <- tibble::as_tibble(grid)
  }

  grid
}
