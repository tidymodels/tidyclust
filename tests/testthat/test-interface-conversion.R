test_that("fit with formula and fit_xy produce same results", {
  set.seed(123)
  fit_formula <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(123)
  fit_xy <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit_xy(mtcars)

  expect_identical(
    extract_cluster_assignment(fit_formula),
    extract_cluster_assignment(fit_xy)
  )
})

test_that("formula with subset of variables works", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~ mpg + cyl + disp, mtcars)

  centroids <- extract_centroids(fit)

  expect_named(centroids, c(".cluster", "mpg", "cyl", "disp"))
  expect_equal(nrow(centroids), 3)
})

test_that("formula interface works for hier_clust", {
  fit <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  expect_s3_class(fit, "cluster_fit")
  expect_equal(nrow(extract_cluster_assignment(fit)), nrow(mtcars))
})

test_that("fit_xy interface works for hier_clust", {
  fit <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit_xy(mtcars)

  expect_s3_class(fit, "cluster_fit")
  expect_equal(nrow(extract_cluster_assignment(fit)), nrow(mtcars))
})

test_that("fit and fit_xy produce same results for hier_clust", {
  fit_formula <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  fit_xy <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit_xy(mtcars)

  expect_identical(
    extract_cluster_assignment(fit_formula),
    extract_cluster_assignment(fit_xy)
  )
})
