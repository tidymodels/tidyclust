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
