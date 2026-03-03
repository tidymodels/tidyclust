test_that("fit and fit_xy errors if outcome is provided", {
  expect_no_error(
    k_means(num_clusters = 5) |> fit_xy(mtcars)
  )

  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 5) |>
      fit_xy(mtcars, y = mtcars$mpg)
  )

  km <- k_means(num_clusters = 5)

  expect_no_error(
    workflows::workflow(~., km) |> fit(mtcars)
  )
  expect_snapshot(
    error = TRUE,
    workflows::workflow(mpg ~ ., km) |> fit(mtcars)
  )
})

test_that("k_means works with num_clusters = 1", {
  fit <- k_means(num_clusters = 1) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- predict(fit, mtcars)

  expect_all_equal(res$.pred_cluster, factor("Cluster_1"))
  expect_identical(levels(res$.pred_cluster), "Cluster_1")
})

test_that("k_means errors when num_clusters > distinct data points", {
  small_data <- mtcars[1:5, 1:3]

  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 10) |>
      set_engine("stats") |>
      fit(~., data = small_data)
  )
})

test_that("hier_clust with cut_height = 0 produces n clusters", {
  fit <- hier_clust(cut_height = 0) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- predict(fit, mtcars)

  expect_identical(length(unique(res$.pred_cluster)), nrow(mtcars))
})
