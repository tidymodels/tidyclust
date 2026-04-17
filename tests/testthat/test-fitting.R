test_that("fit() and fit_xy() errors if outcome is provided", {
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

test_that("k_means() works with num_clusters = 1", {
  fit <- k_means(num_clusters = 1) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- predict(fit, mtcars)

  expect_all_equal(res$.pred_cluster, factor("Cluster_1"))
  expect_identical(levels(res$.pred_cluster), "Cluster_1")
})

test_that("k_means() errors when num_clusters > distinct data points", {
  small_data <- mtcars[1:5, 1:3]

  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 10) |>
      set_engine("stats") |>
      fit(~., data = small_data)
  )
})

test_that("hier_clust() with cut_height = 0 produces n clusters", {
  fit <- hier_clust(cut_height = 0) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- predict(fit, mtcars)

  expect_identical(length(unique(res$.pred_cluster)), nrow(mtcars))
})

test_that("fit() errors when mode is unknown", {
  spec <- k_means(num_clusters = 3)
  spec$mode <- "unknown"

  expect_snapshot(
    error = TRUE,
    fit(spec, ~., data = mtcars)
  )
})

test_that("fit() uses default engine when not set and verbosity > 0", {
  expect_snapshot(
    fit <- k_means(num_clusters = 3, engine = NULL) |>
      fit(~., data = mtcars[1:10, ], control = control_cluster(verbosity = 1))
  )
  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "stats")
})

test_that("fit_xy() uses default engine when not set and verbosity > 0", {
  expect_snapshot(
    fit <- k_means(num_clusters = 3, engine = NULL) |>
      fit_xy(mtcars[1:10, ], control = control_cluster(verbosity = 1))
  )
  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "stats")
})

test_that("fit() errors when called with x and y arguments", {
  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 3) |>
      set_engine("stats") |>
      fit(~., data = mtcars, x = mtcars, y = mtcars$mpg)
  )
})

test_that("fit() errors when formula is not a formula", {
  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 3) |>
      set_engine("stats") |>
      fit("not a formula", data = mtcars)
  )
})

test_that("fit_xy() errors when x has no column names", {
  mat <- as.matrix(mtcars)
  colnames(mat) <- NULL

  expect_snapshot(
    error = TRUE,
    k_means(num_clusters = 3) |>
      set_engine("stats") |>
      fit_xy(mat)
  )
})

test_that("fit_xy() works with matrix input", {
  mat <- as.matrix(mtcars)

  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit_xy(mat)

  expect_s3_class(fit, "cluster_fit")
})
