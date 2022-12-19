test_that("multiplication works", {
  kmeans_spec <- k_means(num_clusters = 1) %>%
    set_engine("stats")

  kmeans_fit <- fit(kmeans_spec, ~., mtcars)

  dists <- mtcars %>%
    as.matrix() %>%
    dist()

  res <- silhouette(kmeans_fit, dists = dists)
  exp_res <- tibble::tibble(
    cluster = rep(factor("Cluster_1"), 32),
    neighbor = rep(factor(NA, levels = "Cluster_1"), 32),
    sil_width = rep(NA_real_, 32)
  )
  expect_identical(res, exp_res)
})
