test_that("cluster_metric_set works", {
  kmeans_spec <- k_means(num_clusters = 5) |>
    set_engine("stats")

  kmeans_fit <- fit(kmeans_spec, ~., mtcars)

  my_metrics <- cluster_metric_set(
    sse_ratio,
    sse_total,
    sse_within_total,
    silhouette_avg
  )

  exp_res <- tibble::tibble(
    .metric = c("sse_ratio", "sse_total", "sse_within_total", "silhouette_avg"),
    .estimator = "standard",
    .estimate = vapply(
      list(
        sse_ratio_vec,
        sse_total_vec,
        sse_within_total_vec,
        silhouette_avg_vec
      ),
      function(x) x(kmeans_fit, new_data = mtcars),
      FUN.VALUE = numeric(1)
    )
  )

  expect_equal(
    my_metrics(kmeans_fit, new_data = mtcars),
    exp_res
  )

  expect_snapshot(error = TRUE, my_metrics(kmeans_fit))

  my_metrics <- cluster_metric_set(sse_ratio, sse_total, sse_within_total)

  expect_equal(
    my_metrics(kmeans_fit, new_data = mtcars),
    exp_res[-4, ]
  )
})

test_that("cluster_metric_set error with wrong input", {
  expect_snapshot(
    error = TRUE,
    cluster_metric_set(mean)
  )

  expect_snapshot(
    error = TRUE,
    cluster_metric_set(sse_ratio, mean)
  )
})

test_that("cluster_metric_set errors with advice for some functions", {
  expect_snapshot(
    error = TRUE,
    cluster_metric_set(silhouette)
  )

  expect_snapshot(
    error = TRUE,
    cluster_metric_set(sse_within)
  )
})
