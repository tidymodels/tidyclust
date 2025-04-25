test_that("tune recipe only", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod_no_tune)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

test_that("tune model only (with recipe)", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  num_clusters_quo <- res_workflow$fit$fit$spec$args$num_clusters
  num_clusters <- rlang::quo_get_expr(num_clusters_quo)

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
  expect_false(identical(num_clusters, expr(tune())))
  expect_true(res_workflow$trained)
})

test_that("tune model only (with variables)", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)

  wflow <- workflows::workflow() |>
    workflows::add_variables(outcomes = NULL, predictors = everything()) |>
    workflows::add_model(helper_objects$kmeans_mod)

  pset <- hardhat::extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  metrics <- cluster_metric_set(sse_total, sse_within_total)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  expect_equal(res$id, folds$id)

  res_est <- tune::collect_metrics(res)

  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
})

test_that("tune model only (with formula)", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)

  wflow <- workflows::workflow() |>
    workflows::add_formula(~.) |>
    workflows::add_model(helper_objects$kmeans_mod)

  pset <- hardhat::extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)

  folds <- rsample::vfold_cv(mtcars, v = 2)

  metrics <- cluster_metric_set(sse_total, sse_within_total)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  expect_equal(res$id, folds$id)

  res_est <- tune::collect_metrics(res)

  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
})

test_that("tune model and recipe", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  num_clusters_quo <- res_workflow$fit$fit$spec$args$num_clusters
  num_clusters <- rlang::quo_get_expr(num_clusters_quo)

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c(
      "num_clusters",
      "num_comp",
      ".metric",
      ".estimator",
      ".estimate",
      ".config"
    )
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
  expect_false(identical(num_clusters, expr(tune())))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

test_that("verbose argument works", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity, verbose = TRUE)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  expect_snapshot(
    res <- tune_cluster(
      wflow,
      resamples = folds,
      grid = grid,
      control = control,
      metrics = metrics
    )
  )
})

test_that('tune model and recipe (parallel_over = "everything")', {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(
    extract = identity,
    parallel_over = "everything"
  )
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c(
      "num_clusters",
      "num_comp",
      ".metric",
      ".estimator",
      ".estimate",
      ".config"
    )
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
})

test_that("tune model only - failure in formula is caught elegantly", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  cars_grid <- tibble::tibble(num_clusters = 2)

  # these terms don't exist!
  expect_snapshot(
    cars_res <- tune_cluster(
      helper_objects$kmeans_mod,
      ~z,
      resamples = data_folds,
      grid = cars_grid,
      control = tune::control_grid(
        extract = function(x) {
          1
        },
        save_pred = TRUE
      )
    )
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # formula failed - no models run
  expect_equal(extracts, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("argument order gives errors for recipes", {
  helper_objects <- helper_objects_tidyclust()

  expect_snapshot(error = TRUE, {
    tune_cluster(
      helper_objects$rec_tune_1,
      helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2)
    )
  })
})

test_that("argument order gives errors for formula", {
  helper_objects <- helper_objects_tidyclust()

  expect_snapshot(error = TRUE, {
    tune_cluster(
      mpg ~ .,
      helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2)
    )
  })
})

test_that("metrics can be NULL", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod_no_tune)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  set.seed(4400)
  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control
  )

  set.seed(4400)
  res1 <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )

  expect_identical(res$.metrics, res1$.metrics)
})

test_that("tune recipe only", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod_no_tune)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid)))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

test_that("ellipses with tune_cluster", {
  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod_no_tune)
  folds <- rsample::vfold_cv(mtcars, v = 2)
  expect_snapshot(
    tune_cluster(wflow, resamples = folds, grid = 3, something = "wrong")
  )
})

test_that("determining the grid type", {
  grid_1 <- expand.grid(a = 1:20, b = letters[1:2])
  expect_true(tune:::is_regular_grid(grid_1))
  expect_true(tune:::is_regular_grid(grid_1[-(1:2), ]))
  expect_false(tune:::is_regular_grid(grid_1[-(1:20), ]))
  set.seed(1932)
  grid_2 <- data.frame(a = runif(length(letters)), b = letters)
  expect_false(tune:::is_regular_grid(grid_2))
})

test_that("retain extra attributes", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)
  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "parameters"))

  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "cluster_metric_set"))
})

test_that("select_best() and show_best() works", {
  helper_objects <- helper_objects_tidyclust()

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 2)
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )

  expect_snapshot(tmp <- tune::show_best(res))

  expect_equal(
    tune::show_best(res, metric = "sse_within_total"),
    tune::collect_metrics(res) |>
      dplyr::filter(.metric == "sse_within_total") |>
      dplyr::slice_min(mean, n = 5, with_ties = FALSE)
  )

  expect_equal(
    tune::show_best(res, metric = "sse_total"),
    tune::collect_metrics(res) |>
      dplyr::filter(.metric == "sse_total") |>
      dplyr::slice_min(mean, n = 5, with_ties = FALSE)
  )

  expect_snapshot(tmp <- tune::select_best(res))

  expect_equal(
    tune::select_best(res, metric = "sse_within_total"),
    tune::collect_metrics(res) |>
      dplyr::filter(.metric == "sse_within_total") |>
      dplyr::slice_min(mean, n = 1, with_ties = FALSE) |>
      dplyr::select(num_clusters, .config)
  )

  expect_equal(
    tune::select_best(res, metric = "sse_total"),
    tune::collect_metrics(res) |>
      dplyr::filter(.metric == "sse_total") |>
      dplyr::slice_min(mean, n = 1, with_ties = FALSE) |>
      dplyr::select(num_clusters, .config)
  )
})

test_that("doesn't error if recipes uses id variables", {
  helper_objects <- helper_objects_tidyclust()

  mtcars_id <- mtcars |>
    tibble::rownames_to_column(var = "model")

  rec_id <- recipes::recipe(~., data = mtcars_id) |>
    recipes::update_role(model, new_role = "id variable") |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  set.seed(4400)
  wflow <- workflows::workflow() |>
    workflows::add_recipe(rec_id) |>
    workflows::add_model(helper_objects$kmeans_mod)
  pset <- hardhat::extract_parameter_set_dials(wflow) |>
    update(num_clusters = dials::num_clusters(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars_id, v = 2)
  control <- tune::control_grid(extract = identity)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control,
    metrics = metrics
  )
  res_est <- tune::collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "sse_total"), nrow(grid))
  expect_equal(sum(res_est$.metric == "sse_within_total"), nrow(grid))
  expect_equal(res_est$n, rep(2, nrow(grid) * 2))
  expect_true(res_workflow$trained)
})
