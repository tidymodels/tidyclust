test_that("fitting", {
  skip_if_not_installed("mclust")

  set.seed(1234)
  spec <- gm_clust(num_clusters = 3) %>%
    set_engine("mclust")

  expect_no_error(
    res <- fit(spec, ~., mtcars)
  )

  expect_no_error(
    res <- fit_xy(spec, mtcars)
  )
})

test_that("predicting", {
  skip_if_not_installed("mclust")

  set.seed(1234)
  spec <- gm_clust(num_clusters = 3) %>%
    set_engine("mclust")

  res <- fit(spec, ~., iris)

  preds <- predict(res, iris[c(25, 75, 125), ])

  expect_identical(
    preds,
    tibble::tibble(.pred_cluster = factor(paste0("Cluster_", 1:3)))
  )
})

test_that("all levels are preserved with 1 row predictions", {
  skip_if_not_installed("mclust")

  set.seed(1234)
  spec <- gm_clust(num_clusters = 3) %>%
    set_engine("mclust")

  res <- fit(spec, ~., mtcars)

  preds <- predict(res, mtcars[1, ])

  expect_identical(
    levels(preds$.pred_cluster),
    paste0("Cluster_", 1:3)
  )
})

test_that("extract_centroids() works", {
  skip_if_not_installed("mclust")

  set.seed(1234)
  spec <- gm_clust(num_clusters = 3) %>%
    set_engine("mclust")

  res <- fit(spec, ~., iris)

  centroids <- extract_centroids(res)

  expect_identical(
    colnames(centroids),
    c(
      ".cluster",
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Speciesversicolor",
      "Speciesvirginica"
    )
  )

  expect_identical(
    centroids$.cluster,
    factor(c("Cluster_1", "Cluster_2", "Cluster_3"))
  )
})

test_that("extract_cluster_assignment() works", {
  set.seed(1234)
  spec <- gm_clust(num_clusters = 3) %>%
    set_engine("mclust")

  res <- fit(spec, ~., iris)

  clusters <- extract_cluster_assignment(res)

  res$fit$classification

  expected <- vctrs::vec_cbind(
    tibble::tibble(
      .cluster = factor(paste0("Cluster_", res$fit$classification))
    )
  )

  expect_identical(
    clusters,
    expected
  )
})

test_that("axe_data replaces data with 0-row matrix and predict still works", {
  skip_if_not_installed("butcher")
  skip_if_not_installed("mclust")

  g_fit <- gm_clust(num_clusters = 3) |>
    set_engine("mclust") |>
    fit(~., data = mtcars[, 1:3])

  g_axed <- butcher::axe_data(g_fit)

  expect_equal(nrow(g_axed$fit$data), 0)
  expect_null(attr(g_axed$fit, "training_data"))
  expect_equal(nrow(predict(g_axed, mtcars[1:5, 1:3])), 5)
})

test_that("axe_fitted removes z/classification/uncertainty and predict still works", {
  skip_if_not_installed("butcher")
  skip_if_not_installed("mclust")

  g_fit <- gm_clust(num_clusters = 3) |>
    set_engine("mclust") |>
    fit(~., data = mtcars[, 1:3])

  g_axed <- butcher::axe_fitted(g_fit)

  expect_equal(nrow(g_axed$fit$z), 0)
  expect_length(g_axed$fit$classification, 0)
  expect_length(g_axed$fit$uncertainty, 0)
  expect_equal(nrow(predict(g_axed, mtcars[1:5, 1:3])), 5)
})

test_that("axe_data reduces serialized size", {
  skip_if_not_installed("butcher")
  skip_if_not_installed("mclust")

  big_data <- data.frame(matrix(rnorm(3000), ncol = 3))
  g_fit <- gm_clust(num_clusters = 3) |>
    set_engine("mclust") |>
    fit(~., data = big_data)

  g_axed <- butcher::axe_data(g_fit)

  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))
  saveRDS(g_fit, f1)
  saveRDS(g_axed, f2)

  expect_lt(file.size(f2), file.size(f1))
})
