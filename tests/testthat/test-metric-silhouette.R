test_that("silhouette() works", {
  kmeans_spec <- k_means(num_clusters = 1) |>
    set_engine("stats")

  kmeans_fit <- fit(kmeans_spec, ~., mtcars)

  dists <- mtcars |>
    as.matrix() |>
    dist()

  res <- silhouette(kmeans_fit, dists = dists)
  exp_res <- tibble::tibble(
    cluster = rep(factor("Cluster_1"), 32),
    neighbor = rep(factor(NA, levels = "Cluster_1"), 32),
    sil_width = rep(NA_real_, 32)
  )
  expect_identical(res, exp_res)
})

test_that("silhouette() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    silhouette(spec)
  )
})

test_that("silhouette_avg() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    silhouette_avg(spec)
  )
})

test_that("silhouette_avg() returns NA for single cluster", {
  fit <- k_means(num_clusters = 1) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  dists <- dist(mtcars)
  res <- silhouette_avg(fit, dists = dists)

  expect_identical(res$.estimate, NA_real_)
})

test_that("silhouette() values are in [-1, 1]", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  dists <- dist(mtcars)
  res <- silhouette(fit, dists = dists)

  expect_true(all(res$sil_width >= -1 | is.na(res$sil_width)))
  expect_true(all(res$sil_width <= 1 | is.na(res$sil_width)))
})

test_that("silhouette_avg() has direction maximize", {
  expect_identical(attr(silhouette_avg, "direction"), "maximize")
})

test_that("silhouette_avg() returns expected structure", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  dists <- dist(mtcars)
  res <- silhouette_avg(fit, dists = dists)

  expect_s3_class(res, "tbl_df")
  expect_named(res, c(".metric", ".estimator", ".estimate"))
  expect_identical(res$.metric, "silhouette_avg")
  expect_identical(res$.estimator, "standard")
  expect_gte(res$.estimate, -1)
  expect_lte(res$.estimate, 1)
})
