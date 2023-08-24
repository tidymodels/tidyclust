test_that("fitting", {
  skip_if_not_installed("clustMixType")

  set.seed(1234)
  spec <- k_means(num_clusters = 3) %>%
    set_engine("clustMixType")

  expect_no_error(
    res <- fit(spec, ~., iris)
  )

  expect_no_error(
    res <- fit_xy(spec, iris)
  )

  expect_identical(res$fit$type, "standard")

  spec <- k_means(num_clusters = 3) %>%
    set_engine("clustMixType", type = "gower")

  res <- fit(spec, ~., iris)

  expect_identical(res$fit$type, "gower")
})

test_that("predicting", {
  skip_if_not_installed("clustMixType")

  set.seed(1234)
  spec <- k_means(num_clusters = 3) %>%
    set_engine("clustMixType")

  res <- fit(spec, ~., iris)

  preds <- predict(res, iris[c(25, 75, 125), ])

  expect_identical(
    preds,
    tibble::tibble(.pred_cluster = factor(paste0("Cluster_", 1:3)))
  )
})

test_that("extract_centroids() works", {
  skip_if_not_installed("clustMixType")

  set.seed(1234)
  spec <- k_means(num_clusters = 3) %>%
    set_engine("clustMixType")

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
  skip_if_not_installed("clustMixType")

  set.seed(1234)
  spec <- k_means(num_clusters = 3) %>%
    set_engine("clustMixType")

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
