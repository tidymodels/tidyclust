test_that("fitting", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  spec <- db_clust(radius = 10, min_points = 5) %>%
    set_engine("dbscan")

  expect_no_error(
    res <- fit(spec, ~., mtcars)
  )

  expect_no_error(
    res <- fit_xy(spec, mtcars)
  )
})

test_that("predicting", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  spec <- db_clust(radius = .42, min_points = 5) %>%
    set_engine("dbscan")

  iris_temp <- iris %>% dplyr::select(-Species)
  res <- fit(spec, ~., iris_temp)


  preds <- predict(res, iris_temp[c(58 ,25, 75, 125), ])
  exp <- factor(c("Outlier", paste0("Cluster_", 1:3)), levels = c("Outlier", paste0("Cluster_", 1:3)))

  expect_identical(
    preds,
    tibble::tibble(.pred_cluster = exp)
  )
})



test_that("all levels are preserved with 1 row predictions", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  spec <- db_clust(radius = 50, min_points = 5) %>%
    set_engine("dbscan")

  res <- fit(spec, ~., mtcars)

  preds <- predict(res, mtcars[1, ])

  expect_identical(
    levels(preds$.pred_cluster),
    c("Outlier", paste0("Cluster_", 1:2))
  )
})

# test_that("extract_centroids() works", {
#   set.seed(1234)
#   spec <- db_clust(radius = .42, min_points = 5) %>%
#     set_engine("dbscan")
#
#   iris_temp <- iris %>% select(-Species)
#   res <- fit(spec, ~., iris)
#
#   centroids <- extract_centroids(res)
#
#   expected <- vctrs::vec_cbind(
#     tibble::tibble(.cluster = factor(paste0("Cluster_", 1:4))),
#     tibble::as_tibble(res$fit$centers)
#   )
#
#   expect_identical(
#     centroids,
#     expected
#   )
# })

test_that("extract_cluster_assignment() works", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  spec <- db_clust(radius = .42, min_points = 5) %>%
    set_engine("dbscan")

  iris_temp <- iris %>% dplyr::select(-Species)
  res <- fit(spec, ~., iris_temp)

  clusters <- extract_cluster_assignment(res)

  expected <- paste0("Cluster_", res$fit$cluster) %>%
    ifelse(. == "Cluster_0", "Outlier", .) %>%
    as.factor() %>%
    relevel(ref = "Outlier") %>%
    tibble::tibble(.cluster = .)


  expect_identical(
    clusters,
    expected
  )
})
