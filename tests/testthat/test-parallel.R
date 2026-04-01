test_that("tune_cluster works with future backend (parallel_over = 'resamples')", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  grid <- dials::grid_regular(
    hardhat::extract_parameter_set_dials(wflow),
    levels = 2
  )
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession(workers = 2))

  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  expect_equal(res$id, folds$id)
  expect_equal(nrow(tune::collect_metrics(res)), nrow(grid) * 2)
})

test_that("tune_cluster works with future backend (parallel_over = 'everything')", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  grid <- dials::grid_regular(
    hardhat::extract_parameter_set_dials(wflow),
    levels = 2
  )
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession(workers = 2))

  res <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(parallel_over = "everything")
  )

  expect_equal(res$id, folds$id)
  expect_equal(nrow(tune::collect_metrics(res)), nrow(grid) * 2)
})

test_that("tune_cluster results match between future and sequential", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  grid <- dials::grid_regular(
    hardhat::extract_parameter_set_dials(wflow),
    levels = 2
  )
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  set.seed(4400)
  res_seq <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(allow_par = FALSE)
  )

  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession(workers = 2))

  set.seed(4400)
  res_par <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    metrics = metrics
  )

  expect_equal(
    tune::collect_metrics(res_seq),
    tune::collect_metrics(res_par)
  )
})

test_that("tune_cluster works with mirai backend", {
  skip_if_not_installed("mirai", minimum_version = "1.0.0")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  grid <- dials::grid_regular(
    hardhat::extract_parameter_set_dials(wflow),
    levels = 2
  )
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  library(mirai)
  daemons(2)
  on.exit(
    {
      daemons(0)
      Sys.sleep(1)
    },
    add = TRUE
  )

  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  expect_equal(res$id, folds$id)
  expect_equal(nrow(tune::collect_metrics(res)), nrow(grid) * 2)
})

test_that("tune_cluster results match between mirai and sequential", {
  skip_if_not_installed("mirai", minimum_version = "1.0.0")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()

  wflow <- workflows::workflow() |>
    workflows::add_recipe(helper_objects$rec_no_tune_1) |>
    workflows::add_model(helper_objects$kmeans_mod)
  grid <- dials::grid_regular(
    hardhat::extract_parameter_set_dials(wflow),
    levels = 2
  )
  grid$num_clusters <- grid$num_clusters + 1
  folds <- rsample::vfold_cv(mtcars, v = 2)
  metrics <- cluster_metric_set(sse_within_total, sse_total)

  set.seed(4400)
  res_seq <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    metrics = metrics,
    control = tune::control_grid(allow_par = FALSE)
  )

  library(mirai)
  daemons(2)
  on.exit(
    {
      daemons(0)
      Sys.sleep(1)
    },
    add = TRUE
  )

  set.seed(4400)
  res_par <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    metrics = metrics
  )

  expect_equal(
    tune::collect_metrics(res_seq),
    tune::collect_metrics(res_par)
  )
})

test_that("errors in parallel workers are caught with future", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_on_cran()

  helper_objects <- helper_objects_tidyclust()
  folds <- rsample::vfold_cv(mtcars, v = 2)
  grid <- tibble::tibble(num_clusters = 2)

  library(future)
  on.exit(plan(sequential), add = TRUE)
  plan(multisession(workers = 2))

  expect_snapshot(
    res <- tune_cluster(
      helper_objects$kmeans_mod,
      ~z,
      resamples = folds,
      grid = grid,
      control = tune::control_grid(save_pred = TRUE)
    )
  )

  notes <- res$.notes
  expect_length(notes, 2L)
  expect_named(notes[[1]], c("location", "type", "note", "trace"))
})
