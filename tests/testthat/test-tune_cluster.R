library(tidymodels)

test_that("tune recipe only", {
  helper_objects <- helper_objects_celery()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$kmeans_mod_no_tune)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  control <- control_grid(extract = identity)
  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control, metrics = metrics)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "tot_sse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "tot_wss"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that("tune model only (with recipe)", {
  helper_objects <- helper_objects_celery()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_no_tune_1) %>%
    add_model(helper_objects$kmeans_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  control <- control_grid(extract = identity)
  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control, metrics = metrics)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  k_quo <- res_workflow$fit$fit$spec$args$k
  k <- rlang::quo_get_expr(k_quo)

  expect_equal(res$id, folds$id)
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "tot_sse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "tot_wss"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(k, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that("tune model only (with variables)", {
  helper_objects <- helper_objects_celery()

  set.seed(4400)

  wflow <- workflow() %>%
    add_variables(mpg, everything()) %>%
    add_model(helper_objects$kmeans_mod)

  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)

  folds <- rsample::vfold_cv(mtcars)

  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  expect_equal(res$id, folds$id)

  res_est <- collect_metrics(res)

  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "tot_sse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "tot_wss"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that("tune model and recipe", {
  helper_objects <- helper_objects_celery()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$kmeans_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  control <- control_grid(extract = identity)
  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control, metrics = metrics)
  res_est <- collect_metrics(res)
  res_workflow <- res$.extracts[[1]]$.extracts[[1]]

  # Ensure tunable parameters in spec are finalized
  k_quo <- res_workflow$fit$fit$spec$args$k
  k <- rlang::quo_get_expr(k_quo)

  # Ensure tunable parameters in recipe are finalized
  num_comp <- res_workflow$pre$actions$recipe$recipe$steps[[2]]$num_comp

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("k", "num_comp", ".metric", ".estimate", ".estimator", ".config")
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "tot_sse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "tot_wss"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
  expect_false(identical(k, expr(tune())))
  expect_false(identical(num_comp, expr(tune())))
  expect_true(res_workflow$trained)
})

# ------------------------------------------------------------------------------

test_that('tune model and recipe (parallel_over = "everything")', {
  helper_objects <- helper_objects_celery()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$kmeans_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  control <- control_grid(extract = identity, parallel_over = "everything")
  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control, metrics = metrics)
  res_est <- collect_metrics(res)

  expect_equal(res$id, folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("k", "num_comp", ".metric", ".estimate", ".estimator", ".config")
  )
  expect_equal(nrow(res_est), nrow(grid) * 2)
  expect_equal(sum(res_est$.metric == "tot_sse"), nrow(grid))
  expect_equal(sum(res_est$.metric == "tot_wss"), nrow(grid))
  expect_equal(res_est$n, rep(10, nrow(grid) * 2))
})

# ------------------------------------------------------------------------------

test_that("tune recipe only - failure in recipe is caught elegantly", {
  skip("wait for parameter checking")
  helper_objects <- helper_objects_celery()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())

  model <- helper_objects$kmeans_mod_no_tune

  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(3, NA_real_, 4))

  # ask for predictions and extractions
  control <- control_grid(
    save_pred = TRUE,
    extract = function(x) 1L
  )

  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)

  suppressMessages({
    cars_res <- tune_cluster(
      model,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control,
      metrics = metrics
    )
  })

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extract <- cars_res$.extracts[[1]]

  predictions <- cars_res$.predictions[[1]]
  used_deg_free <- sort(unique(predictions$deg_free))

  expect_length(notes, 2L)

  # failing rows are not in the output
  expect_equal(nrow(extract), 2L)
  expect_equal(extract$deg_free, c(3, 4))

  expect_equal(used_deg_free, c(3, 4))
})

test_that("tune model only - failure in recipe is caught elegantly", {
  skip("wait for parameter checking")
  helper_objects <- helper_objects_celery()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  # NA values not allowed in recipe
  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = NA_real_)

  cars_grid <- tibble(cost = c(0.01, 0.02))

  expect_snapshot(
    cars_res <- tune_cluster(
      helper_objects$svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    )
  )

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extracts <- cars_res$.extracts
  predictions <- cars_res$.predictions

  expect_length(notes, 2L)

  # recipe failed - no models run
  expect_equal(extracts, list(NULL, NULL))
  expect_equal(predictions, list(NULL, NULL))
})

test_that("tune model only - failure in formula is caught elegantly", {
  helper_objects <- helper_objects_celery()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  cars_grid <- tibble(cost = 0.01)

  # these terms don't exist!
  expect_snapshot(
    cars_res <- tune_cluster(
      helper_objects$kmeans_mod,
      y ~ z,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
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

test_that("tune model and recipe - failure in recipe is caught elegantly", {
  skip("wait for parameter checking")
  helper_objects <- helper_objects_celery()

  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)

  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_bs(disp, deg_free = tune())


  # NA values not allowed in recipe
  cars_grid <- tibble(deg_free = c(NA_real_, 10L), cost = 0.01)

  suppressMessages({
    cars_res <- tune_cluster(
      helper_objects$svm_mod,
      preprocessor = rec,
      resamples = data_folds,
      grid = cars_grid,
      control = control_grid(extract = function(x) {1}, save_pred = TRUE)
    )
  })

  notes <- cars_res$.notes
  note <- notes[[1]]$note

  extract <- cars_res$.extracts[[1]]
  prediction <- cars_res$.predictions[[1]]

  expect_length(notes, 2L)

  # recipe failed half of the time, only 1 model passed
  expect_equal(nrow(extract), 1L)
  expect_equal(extract$deg_free, 10L)
  expect_equal(extract$cost, 0.01)

  expect_equal(
    unique(prediction[, c("deg_free", "cost")]),
    tibble(deg_free = 10, cost = 0.01)
  )
})

test_that("argument order gives errors for recipes", {
  helper_objects <- helper_objects_celery()

  expect_snapshot(error = TRUE, {
    tune_cluster(
      helper_objects$rec_tune_1,
      helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2)
    )
  })
})

test_that("argument order gives errors for formula", {
  helper_objects <- helper_objects_celery()

  expect_snapshot(error = TRUE, {
    tune_cluster(mpg ~ ., helper_objects$kmeans_mod_no_tune, rsample::vfold_cv(mtcars, v = 2))
  })
})

test_that("ellipses with tune_cluster", {
  helper_objects <- helper_objects_celery()

  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$kmeans_mod_no_tune)
  folds <- rsample::vfold_cv(mtcars)
  expect_snapshot(
    tune_cluster(wflow, resamples = folds, grid = 3, something = "wrong")
  )
})


test_that("determining the grid type", {
  grid_1 <- expand.grid(a = 1:100, b = letters[1:2])
  expect_true(tune:::is_regular_grid(grid_1))
  expect_true(tune:::is_regular_grid(grid_1[-(1:10), ]))
  expect_false(tune:::is_regular_grid(grid_1[-(1:100), ]))
  set.seed(1932)
  grid_2 <- data.frame(a = runif(length(letters)), b = letters)
  expect_false(tune:::is_regular_grid(grid_2))
})

test_that("retain extra attributes", {
  skip("wait for parameter checking")
  helper_objects <- helper_objects_celery()

  set.seed(4400)
  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_no_tune_1) %>%
    add_model(helper_objects$kmeans_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  metrics <- list(tot_wss = tot_wss, tot_sse = tot_sse)
  res <- tune_cluster(wflow, resamples = folds, grid = grid, metrics = metrics)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "parameters"))

  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  set.seed(4400)
  wflow <- workflow() %>%
    add_formula(mpg ~ .) %>%
    add_model(helper_objects$svm_mod)
  pset <- extract_parameter_set_dials(wflow)
  grid <- dials::grid_regular(pset, levels = 3)
  folds <- rsample::vfold_cv(mtcars)
  res <- tune_cluster(wflow, resamples = folds, grid = grid)

  att <- attributes(res)
  att_names <- names(att)
  expect_true(any(att_names == "metrics"))
  expect_true(any(att_names == "parameters"))

  expect_true(inherits(att$parameters, "parameters"))
  expect_true(inherits(att$metrics, "metric_set"))

  res2 <- tune_cluster(
    wflow,
    resamples = folds,
    grid = grid,
    control = control_grid(save_workflow = TRUE)
  )
  expect_null(attr(res, "workflow"))
  expect_true(inherits(attr(res2, "workflow"), "workflow"))

  wflow2 <- workflow() %>%
    add_recipe(recipes::recipe(mpg ~ ., mtcars[rep(1:32, 3000), ])) %>%
    add_model(helper_objects$svm_mod)
  pset2 <- extract_parameter_set_dials(wflow2)
  grid2 <- dials::grid_regular(pset2, levels = 3)

  expect_message(
    tune_cluster(
      wflow2,
      resamples = folds,
      grid = grid2,
      control = control_grid(save_workflow = TRUE)
    ),
    "being saved contains a recipe, which is"
  )
})
