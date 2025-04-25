test_that("fitting", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  ames_cat <- dplyr::select(ames, dplyr::where(is.factor))

  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  expect_no_error(
    res <- fit(spec, ~., ames_cat)
  )

  expect_no_error(
    res <- fit_xy(spec, ames_cat)
  )

  expect_identical(res$fit$weighted, FALSE)

  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR", weighted = TRUE)

  res <- fit(spec, ~., ames_cat)

  expect_identical(res$fit$weighted, TRUE)
})

test_that("predicting", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  ames_cat <- dplyr::select(ames, dplyr::where(is.factor))

  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  res <- fit(spec, ~., ames_cat)

  preds <- predict(res, ames_cat[c(1:5), ])

  expect_identical(
    preds,
    tibble::tibble(
      .pred_cluster = factor(
        paste0("Cluster_", c(1, 1, 1, 1, 2)),
        paste0("Cluster_", 1:3)
      )
    )
  )
})

test_that("all levels are preserved with 1 row predictions", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  ames_cat <- dplyr::select(ames, dplyr::where(is.factor))

  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  res <- fit(spec, ~., ames_cat)

  preds <- predict(res, ames_cat[1, ])

  expect_identical(
    levels(preds$.pred_cluster),
    paste0("Cluster_", 1:3)
  )
})

test_that("predicting ties argument works", {
  skip_if_not_installed("klaR")

  dat <- data.frame(
    x = c("A", "A", "B", "B", "C"),
    y = c("A", "A", "B", "B", "C")
  )

  set.seed(1234)
  spec <- k_means(num_clusters = 2) |>
    set_engine("klaR")

  res <- fit(spec, ~., dat)

  expect_identical(
    predict(res, data.frame(x = "C", y = "C"), ties = "first"),
    tibble::tibble(.pred_cluster = factor("Cluster_1", paste0("Cluster_", 1:2)))
  )

  expect_identical(
    predict(res, data.frame(x = "C", y = "C"), ties = "last"),
    tibble::tibble(.pred_cluster = factor("Cluster_2", paste0("Cluster_", 1:2)))
  )
})

test_that("extract_centroids() works", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  ames_cat <- dplyr::select(ames, dplyr::where(is.factor))

  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  res <- fit(spec, ~., ames_cat)

  centroids <- extract_centroids(res)

  expected <- vctrs::vec_cbind(
    tibble::tibble(.cluster = factor(paste0("Cluster_", 1:3))),
    tibble::as_tibble(res$fit$modes)
  )

  expect_identical(
    centroids,
    expected
  )
})

test_that("extract_cluster_assignment() works", {
  skip_if_not_installed("klaR")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  ames_cat <- dplyr::select(ames, dplyr::where(is.factor))

  set.seed(1234)
  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  res <- fit(spec, ~., ames_cat)

  clusters <- extract_cluster_assignment(res)

  exp_cluster <- res$fit$cluster
  exp_cluster <- order(unique(exp_cluster))[exp_cluster]

  expected <- vctrs::vec_cbind(
    tibble::tibble(.cluster = factor(paste0("Cluster_", exp_cluster)))
  )

  expect_identical(
    clusters,
    expected
  )
})
