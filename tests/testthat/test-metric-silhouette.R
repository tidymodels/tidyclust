test_that("silhouette works", {
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
