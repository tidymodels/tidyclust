test_that("partition predictions", {
  kmeans_fit <-
    k_means(num_clusters = 3, mode = "partition") |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_true(tibble::is_tibble(predict(kmeans_fit, new_data = mtcars)))
  expect_true(
    is.factor(
      tidyclust:::predict_cluster.cluster_fit(kmeans_fit, new_data = mtcars)
    )
  )
  expect_equal(names(predict(kmeans_fit, new_data = mtcars)), ".pred_cluster")
})
