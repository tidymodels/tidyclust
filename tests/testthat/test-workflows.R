test_that("integrates with workflows::add_variables()", {
  skip_if_not_installed("workflows")

  kmeans_spec <- k_means(num_clusters = 2)

  wf_spec <- workflows::workflow() |>
    workflows::add_variables(outcomes = NULL, predictors = everything()) |>
    workflows::add_model(kmeans_spec)

  expect_no_error(
    fit(wf_spec, data = mtcars)
  )

  wf_spec <- workflows::workflow() |>
    workflows::add_variables(outcomes = mpg, predictors = everything()) |>
    workflows::add_model(kmeans_spec)

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, data = mtcars)
  )
})

test_that("integrates with workflows::add_formula()", {
  skip_if_not_installed("workflows")

  kmeans_spec <- k_means(num_clusters = 2)

  wf_spec <- workflows::workflow() |>
    workflows::add_formula(~.) |>
    workflows::add_model(kmeans_spec)

  expect_no_error(
    fit(wf_spec, data = mtcars)
  )

  wf_spec <- workflows::workflow() |>
    workflows::add_formula(mpg ~ .) |>
    workflows::add_model(kmeans_spec)

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, data = mtcars)
  )
})

test_that("integrates with workflows::add_recipe()", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  kmeans_spec <- k_means(num_clusters = 2)

  wf_spec <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(~., data = mtcars)) |>
    workflows::add_model(kmeans_spec)

  expect_no_error(
    fit(wf_spec, data = mtcars)
  )

  wf_spec <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(mpg ~ ., data = mtcars)) |>
    workflows::add_model(kmeans_spec)

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, data = mtcars)
  )
})

test_that("workflow with recipe preprocessing", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(k_means(num_clusters = 3))

  wf_fit <- fit(wf, mtcars)

  expect_s3_class(wf_fit, "workflow")
  expect_true(wf_fit$trained)
})

test_that("extract_centroids works on workflows", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(k_means(num_clusters = 3))

  wf_fit <- fit(wf, mtcars)

  centroids <- extract_centroids(wf_fit)

  expect_s3_class(centroids, "tbl_df")
  expect_equal(nrow(centroids), 3)
  expect_contains(names(centroids), ".cluster")
})

test_that("extract_cluster_assignment works on workflows", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(k_means(num_clusters = 3))

  wf_fit <- fit(wf, mtcars)

  assignments <- extract_cluster_assignment(wf_fit)

  expect_s3_class(assignments, "tbl_df")
  expect_equal(nrow(assignments), nrow(mtcars))
  expect_named(assignments, ".cluster")
})

test_that("sse_within_total works on workflow fits", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(k_means(num_clusters = 3))

  wf_fit <- fit(wf, mtcars)

  res <- sse_within_total(wf_fit)

  expect_s3_class(res, "tbl_df")
  expect_identical(res$.metric, "sse_within_total")
})

test_that("silhouette_avg works on workflow fits", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(k_means(num_clusters = 3))

  wf_fit <- fit(wf, mtcars)

  prepped_data <- recipes::bake(recipes::prep(rec), mtcars)
  dists <- dist(prepped_data)

  res <- silhouette_avg(wf_fit, dists = dists)

  expect_s3_class(res, "tbl_df")
  expect_identical(res$.metric, "silhouette_avg")
})
