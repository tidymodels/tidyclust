test_that("k_means predictions match raw stats::kmeans predictions", {
  set.seed(1234)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  centroids <- as.matrix(extract_centroids(fit)[, -1])
  raw_preds <- unname(apply(flexclust::dist2(centroids, mtcars), 2, which.min))

  tidyclust_preds <- as.integer(predict(fit, mtcars)$.pred_cluster)

  expect_identical(tidyclust_preds, raw_preds)
})

test_that("k_means predictions match raw ClusterR predictions", {
  skip_if_not_installed("ClusterR")

  set.seed(1234)
  fit <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  centroids <- as.matrix(extract_centroids(fit)[, -1])
  raw_preds <- unname(apply(flexclust::dist2(centroids, mtcars), 2, which.min))

  tidyclust_preds <- as.integer(predict(fit, mtcars)$.pred_cluster)

  expect_identical(tidyclust_preds, raw_preds)
})

test_that("hier_clust predictions return valid clusters", {
  fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  predictions <- predict(fit, mtcars)

  expect_s3_class(predictions$.pred_cluster, "factor")
  expect_identical(nlevels(predictions$.pred_cluster), 4L)
  expect_identical(nrow(predictions), nrow(mtcars))
})

test_that("predictions assign to nearest centroid for k_means", {
  set.seed(1234)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  centroids <- as.matrix(extract_centroids(fit)[, -1])
  predictions <- predict(fit, mtcars)

  for (i in seq_len(nrow(mtcars))) {
    obs <- as.numeric(mtcars[i, ])
    dists <- apply(centroids, 1, function(c) sum((obs - c)^2))
    nearest <- which.min(dists)
    expect_identical(as.integer(predictions$.pred_cluster[i]), nearest)
  }
})

test_that("training data predictions match assignments for k_means", {
  set.seed(1234)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  assignments <- extract_cluster_assignment(fit)
  predictions <- predict(fit, mtcars)

  expect_identical(
    as.character(assignments$.cluster),
    as.character(predictions$.pred_cluster)
  )
})
