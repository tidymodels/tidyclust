test_that("fitting", {
  set.seed(1234)
  spec <- k_means(num_clusters = 5) |>
    set_engine("stats")

  expect_no_error(
    res <- fit(spec, ~., mtcars)
  )

  expect_no_error(
    res <- fit_xy(spec, mtcars)
  )
})

test_that("predicting", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  res <- fit(spec, ~., iris)

  preds <- predict(res, iris[c(25, 75, 125), ])

  expect_identical(
    preds,
    tibble::tibble(.pred_cluster = factor(paste0("Cluster_", 1:3)))
  )
})

test_that("all levels are preserved with 1 row predictions", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  res <- fit(spec, ~., mtcars)

  preds <- predict(res, mtcars[1, ])

  expect_identical(
    levels(preds$.pred_cluster),
    paste0("Cluster_", 1:3)
  )
})

test_that("extract_centroids() works", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  res <- fit(spec, ~., iris)

  centroids <- extract_centroids(res)

  expected <- vctrs::vec_cbind(
    tibble::tibble(.cluster = factor(paste0("Cluster_", 1:3))),
    tibble::as_tibble(res$fit$centers)
  )

  expect_identical(
    centroids,
    expected
  )
})

test_that("extract_cluster_assignment() works", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  res <- fit(spec, ~., iris)

  clusters <- extract_cluster_assignment(res)

  expected <- vctrs::vec_cbind(
    tibble::tibble(.cluster = factor(paste0("Cluster_", res$fit$cluster)))
  )

  expect_identical(
    clusters,
    expected
  )
})

test_that("axe_data removes training_data and predict still works", {
  skip_if_not_installed("butcher")

  set.seed(1234)
  k_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::axe_data(k_fit)

  expect_null(attr(k_axed$fit, "training_data"))
  expect_equal(nrow(predict(k_axed, mtcars)), 32)
})

test_that("axe_fitted empties cluster assignments and predict still works", {
  skip_if_not_installed("butcher")

  set.seed(1234)
  k_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::axe_fitted(k_fit)

  expect_length(k_axed$fit$cluster, 0)
  expect_equal(nrow(predict(k_axed, mtcars)), 32)
})

test_that("axe_data reduces serialized size", {
  skip_if_not_installed("butcher")

  big_data <- data.frame(matrix(rnorm(1e4), ncol = 10))
  k_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = big_data)

  k_axed <- butcher::axe_data(k_fit)

  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))
  saveRDS(k_fit, f1)
  saveRDS(k_axed, f2)

  expect_lt(file.size(f2), file.size(f1))
})
