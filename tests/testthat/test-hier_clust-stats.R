test_that("fitting", {
  set.seed(1234)
  spec <- hier_clust(num_clusters = 3) |>
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
  spec <- hier_clust(num_clusters = 3) |>
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
  spec <- hier_clust(num_clusters = 3) |>
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
  spec <- hier_clust(num_clusters = 3) |>
    set_engine("stats")

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
  spec <- hier_clust(num_clusters = 3) |>
    set_engine("stats")

  res <- fit(spec, ~., iris)

  clusters <- extract_cluster_assignment(res)

  expected <- vctrs::vec_cbind(
    tibble::tibble(
      .cluster = factor(paste0("Cluster_", cutree(res$fit, k = 3)))
    )
  )

  expect_identical(
    clusters,
    expected
  )
})
