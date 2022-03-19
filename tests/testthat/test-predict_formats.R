library(testthat)
library(celery)
library(tibble)
library(dplyr)

# ------------------------------------------------------------------------------

kmeans_fit_stats <-
  k_means(k = 3, mode = "partition") %>%
  set_engine_celery("stats") %>%
  fit(~ ., data = mtcars)


kmeans_fit_cR <-
  k_means(k = 3, mode = "partition") %>%
  set_engine_celery("ClusterR") %>%
  fit(~ ., data = mtcars)

# ------------------------------------------------------------------------------

test_that('kmeans predictions', {
  expect_true(is_tibble(predict(kmeans_fit_stats, new_data = mtcars)))
  expect_true(is.factor(celery:::predict_cluster.cluster_fit(kmeans_fit_stats, new_data = mtcars)))
  expect_equal(names(predict(kmeans_fit_stats, new_data = mtcars)), ".pred_cluster")
})

test_that('predictions match original assignments', {

  expect_equal(predict(kmeans_fit_stats, new_data = mtcars)$.pred_cluster,
               unname(extract_cluster_assignment(kmeans_fit_stats)$.cluster))

  expect_equal(predict(kmeans_fit_cR, new_data = mtcars)$.pred_cluster,
               unname(extract_cluster_assignment(kmeans_fit_cR)$.cluster))

})


test_that('cluster assignments are named right', {

  preds_stats <- extract_cluster_assignment(kmeans_fit_stats)$.cluster
  expect_equal(as.character(unique(preds_stats)), levels(preds_stats))

  preds_CR <- extract_cluster_assignment(kmeans_fit_cR)$.cluster
  expect_equal(as.character(unique(preds_CR)), levels(preds_CR))

  expect_equal(as.character(preds_stats[1]), "Cluster_1")
})

