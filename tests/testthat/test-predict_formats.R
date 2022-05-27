library(testthat)
library(celery)
library(tibble)
library(dplyr)

# ------------------------------------------------------------------------------

kmeans_fit <-
  k_means(k = 3, mode = "partition") %>%
  set_engine_celery("stats") %>%
  fit(~., data = mtcars)

# ------------------------------------------------------------------------------

test_that("regression predictions", {
  expect_true(is_tibble(predict(kmeans_fit, new_data = mtcars)))
  expect_true(is.factor(celery:::predict_cluster.cluster_fit(kmeans_fit, new_data = mtcars)))
  expect_equal(names(predict(kmeans_fit, new_data = mtcars)), ".pred_cluster")
})
