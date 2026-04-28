# https://github.com/tidymodels/tune/blob/main/R/tune_grid.R

#' Model tuning via grid search
#'
#' [tune_cluster()] computes a set of performance metrics for a pre-defined set
#' of tuning parameters that correspond to a cluster model or recipe across one
#' or more resamples of the data.
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
#'   execution. The `.notes` column is a tibble with columns `location`,
#'   `type`, `note`, and `trace`. The `trace` column contains
#'   [rlang::trace_back()] objects for errors and warnings, which can be
#'   useful for debugging.
#'
#' @section Choosing metrics:
#'
#' The `metrics` argument accepts a [cluster_metric_set()]. If `NULL`, the
#' default metrics are [sse_within_total()] and [sse_total()].
#'
#' Common metrics and their interpretation:
#' - [sse_within_total()]: Total within-cluster sum of squares. Lower values
#'   indicate tighter, more compact clusters. Use the "elbow method" — plot
#'   this against `num_clusters` and look for where the improvement flattens.
#' - [sse_ratio()]: Ratio of within-cluster SS to total SS. Lower is better
#'   (more variance explained by the clustering).
#' - [silhouette_avg()]: Average silhouette width (range -1 to 1). Higher
#'   values indicate better-separated clusters. Values above 0.5 are generally
#'   considered good.
#'
#' After tuning, use these functions to inspect results:
#' - [tune::collect_metrics()]: All metrics for every parameter combination.
#' - [tune::show_best()]: Top N parameter combinations for a given metric.
#' - [tune::select_best()]: Single best parameter combination.
#'
#' @section Configuration column:
#' The `.config` column in the results follows the pattern
#' `pre{num}_mod{num}_post{num}`. The numbers encode which combination of
#' preprocessor, model, and postprocessor parameters was used. A value of
#' `0` means that element was not tuned. For example, `pre0_mod2_post0`
#' means the preprocessor was not tuned and this is the second model
#' parameter combination.
#'
#' @section Parallel processing:
#' Parallel processing is supported via the `future` and `mirai` packages.
#' To enable parallelism, set up a `future` plan or `mirai` daemons before
#' calling `tune_cluster()`:
#'
#' ```r
#' # Using future
#' library(future)
#' plan(multisession, workers = 4)
#' res <- tune_cluster(wflow, resamples = folds, grid = grid)
#' plan(sequential)
#'
#' # Using mirai
#' library(mirai)
#' daemons(4)
#' res <- tune_cluster(wflow, resamples = folds, grid = grid)
#' daemons(0)
#' ```
#'
#' See [tune::parallelism] for more details.
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
#' @importFrom tune eval_mirai
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

  if (inherits(resamples, "apparent")) {
    cli::cli_warn(
      c(
        "{.fn tune_cluster} was passed an {.fn apparent} resample.",
        "i" = "Metrics from apparent resamples are excluded when summarizing \\
               with {.code collect_metrics(summarize = TRUE)} (the default). \\
               Use {.code collect_metrics(summarize = FALSE)} to see \\
               per-resample metrics."
      )
    )
  }

  metrics <- check_metrics(metrics, workflow)

  pset <- tune::check_parameters(
    wflow = workflow,
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

  # Save rset attributes
  rset_info <- tune::pull_rset_attributes(resamples)

  resamples <- tune_cluster_loop(
    resamples = resamples,
    grid = grid,
    workflow = workflow,
    param_info = pset,
    metrics = metrics,
    control = control
  )

  if (tune::.is_cataclysmic(resamples)) {
    cli::cli_warn(
      c(
        "All models failed.",
        "i" = "See the {.code .notes} column."
      )
    )
  }

  workflow <- tune::.set_workflow(workflow, control)

  new_tune_results(
    x = resamples,
    parameters = pset,
    metrics = metrics,
    rset_info = rset_info,
    workflow = workflow
  )
}

# ------------------------------------------------------------------------------

tune_cluster_loop <- function(
  resamples,
  grid,
  workflow,
  param_info,
  metrics,

  control
) {
  if (is.null(grid)) {
    grid <- tibble::tibble()
  }

  control <- tune::.update_parallel_over(control, resamples, grid)

  # Determine how to process the tasks
  strategy <- tune::choose_framework(workflow, control)

  # Generate parallel seeds
  resamples$.seeds <- tune::get_parallel_seeds(nrow(resamples))

  # Save rset attributes
  rset_info <- tune::pull_rset_attributes(resamples)
  split_args <- rsample::.get_split_args(resamples)

  resamples <- tune::new_bare_tibble(resamples)
  resamples <- vec_list_rowwise(resamples)

  # Package loading
  tm_pkgs <- c(
    "rsample",
    "workflows",
    "hardhat",
    "tidyclust",
    "parsnip",
    "tune"
  )
  load_pkgs <- c(required_pkgs(workflow), control$pkgs, tm_pkgs)
  load_pkgs <- unique(load_pkgs)

  is_inst <- purrr::map_lgl(load_pkgs, rlang::is_installed)
  if (any(!is_inst)) {
    nms <- load_pkgs[!is_inst]
    cli::cli_abort(
      "Some package installs are needed: {.pkg {nms}}",
      call = NULL
    )
  }

  par_opt <- list()

  # Create static object using tune's .make_static
  static <- tune::.make_static(
    workflow,
    param_info = param_info,
    grid = grid,
    metrics = metrics,
    eval_time = NULL,
    split_args = split_args,
    control = control,
    pkgs = load_pkgs,
    strategy = strategy
  )

  # Handle parallel_over = "everything"
  if (control$parallel_over == "everything") {
    candidates <- purrr::map(seq_len(nrow(grid)), \(i) grid[i, ])
    inds <- tidyr::crossing(s = seq_along(candidates), b = seq_along(resamples))
    inds <- vec_list_rowwise(inds)
  }

  # Use tune's loop_call directly
  cl <- tune::loop_call(control$parallel_over, strategy, par_opt)
  res <- rlang::eval_bare(cl)

  # Process results
  res <- dplyr::bind_rows(res)

  resamples <- dplyr::bind_rows(resamples)
  id_cols <- grep("^id", names(resamples), value = TRUE)

  if (control$parallel_over == "resamples") {
    res <- dplyr::full_join(resamples, res, by = id_cols)
  } else {
    res <- res |>
      dplyr::summarize(
        dplyr::across(dplyr::matches("^\\."), ~ list(purrr::list_rbind(.x))),
        .by = c(!!!id_cols)
      ) |>
      dplyr::full_join(resamples, by = id_cols)
  }

  res <- res |>
    dplyr::select(-dplyr::any_of(".seeds")) |>
    dplyr::select(-dplyr::any_of("outcome_names")) |>
    dplyr::relocate(
      splits,
      dplyr::starts_with("id"),
      .metrics,
      .notes,
      dplyr::any_of(".extracts")
    )

  res
}

vec_list_rowwise <- function(x) {
  vctrs::vec_split(x, by = 1:nrow(x))$val
}

# ------------------------------------------------------------------------------

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


# https://github.com/tidymodels/tune/blob/main/R/checks.R#L274
check_workflow <- function(x, pset = NULL, check_dials = FALSE) {
  if (!inherits(x, "workflow")) {
    cli::cli_abort(
      "The {.arg object} argument should be a {.cls workflow} object."
    )
  }

  if (!tune::.has_preprocessor(x)) {
    cli::cli_abort("A formula, recipe, or variables preprocessor is required.")
  }

  if (!tune::.has_spec(x)) {
    cli::cli_abort("A tidyclust model is required.")
  }

  if (check_dials) {
    if (is.null(pset)) {
      pset <- hardhat::extract_parameter_set_dials(x)
    }

    tune::.check_param_objects(pset)

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
