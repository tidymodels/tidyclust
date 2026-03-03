test_that("extract_fit_engine works for k_means with stats engine", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- extract_fit_engine(fit)

  expect_s3_class(res, "kmeans")
})

test_that("extract_fit_engine works for k_means with ClusterR engine", {
  skip_if_not_installed("ClusterR")

  fit <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  res <- extract_fit_engine(fit)

  expect_s3_class(res, "KMeansCluster")
})

test_that("extract_fit_engine works for hier_clust with stats engine", {
  fit <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- extract_fit_engine(fit)

  expect_s3_class(res, "hclust")
})
