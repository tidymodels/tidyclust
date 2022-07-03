test_that("cluster_metric_set works", {
  kmeans_spec <- k_means(k = 5) %>%
    set_engine_tidyclust("stats")

  kmeans_fit <- fit(kmeans_spec, ~., mtcars)

  my_metrics <- cluster_metric_set(sse_ratio, tot_sse, tot_wss, avg_silhouette)

  exp_res <- tibble::tibble(
    .metric = c("sse_ratio", "tot_sse", "tot_wss", "avg_silhouette"),
    .estimator = "standard",
    .estimate = vapply(
      list(sse_ratio_vec, tot_sse_vec, tot_wss_vec, avg_silhouette_vec),
      function(x) x(kmeans_fit, new_data = mtcars),
      FUN.VALUE = numeric(1)
    )
  )

  expect_equal(
    my_metrics(kmeans_fit, new_data = mtcars),
    exp_res
  )

  expect_snapshot(error = TRUE, my_metrics(kmeans_fit))

  my_metrics <- cluster_metric_set(sse_ratio, tot_sse, tot_wss)

  expect_equal(
    my_metrics(kmeans_fit),
    exp_res[-4, ]
  )
})

test_that("cluster_metric_set error with wrong input", {
  expect_snapshot(error = TRUE,
    cluster_metric_set(mean)
  )

  expect_snapshot(error = TRUE,
    cluster_metric_set(sse_ratio, mean)
  )
})
