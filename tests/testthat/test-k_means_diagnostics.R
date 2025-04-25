test_that("kmeans sse metrics work", {
  set.seed(1234)
  kmeans_fit_stats <- k_means(num_clusters = mtcars[1:3, ]) |>
    set_engine("stats", algorithm = "MacQueen") |>
    fit(~., mtcars)

  # We don't use CENTROIDS argument becuase it breaks testing for different
  # versions of ClusterR #186
  kmeans_fit_ClusterR <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., mtcars)

  km_orig <- kmeans(mtcars, centers = mtcars[1:3, ], algorithm = "MacQueen")
  km_orig_2 <- ClusterR::KMeans_rcpp(
    data = mtcars,
    clusters = 3
  )

  expect_equal(
    sse_within(kmeans_fit_stats)$wss,
    c(42877.103, 76954.010, 7654.146), # hard coded because of order
    tolerance = 0.005
  )

  expect_equal(
    sse_within_total_vec(kmeans_fit_stats),
    km_orig$tot.withinss,
    tolerance = 0.005
  )
  expect_equal(
    sse_total_vec(kmeans_fit_stats),
    km_orig$totss,
    tolerance = 0.005
  )
  expect_equal(
    sse_ratio_vec(kmeans_fit_stats),
    km_orig$tot.withinss / km_orig$totss,
    tolerance = 0.005
  )

  expect_equal(
    sse_within(kmeans_fit_ClusterR)$wss,
    c(42877.103, 56041.432, 4665.041), # hard coded because of order
    tolerance = 0.005
  )

  expect_equal(
    sse_within_total_vec(kmeans_fit_ClusterR),
    sum(km_orig_2$WCSS_per_cluster),
    tolerance = 0.005
  )
  expect_equal(
    sse_total_vec(kmeans_fit_ClusterR),
    sse_total_vec(kmeans_fit_stats),
    tolerance = 0.005
  )
  expect_equal(
    sse_ratio_vec(kmeans_fit_ClusterR),
    0.1661624,
    tolerance = 0.005
  )
})

test_that("kmeans sse metrics work on new data", {
  kmeans_fit_stats <- k_means(num_clusters = mtcars[1:3, ]) |>
    set_engine("stats", algorithm = "MacQueen") |>
    fit(~., mtcars)

  new_data <- mtcars[1:4, ]

  expect_equal(
    sse_within(kmeans_fit_stats, new_data)$wss,
    c(2799.21, 12855.17),
    tolerance = 0.005
  )

  expect_equal(
    sse_within_total_vec(kmeans_fit_stats, new_data),
    15654.38,
    tolerance = 0.005
  )
  expect_equal(
    sse_total_vec(kmeans_fit_stats, new_data),
    32763.7,
    tolerance = 0.005
  )
  expect_equal(
    sse_ratio_vec(kmeans_fit_stats, new_data),
    15654.38 / 32763.7,
    tolerance = 0.005
  )
})

test_that("kmeans sihouette metrics work", {
  kmeans_fit_stats <- k_means(num_clusters = mtcars[1:3, ]) |>
    set_engine("stats", algorithm = "MacQueen") |>
    fit(~., mtcars)

  kmeans_fit_ClusterR <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., mtcars)

  new_data <- mtcars[1:4, ]

  dists <- mtcars |>
    as.matrix() |>
    dist()

  expect_equal(
    names(silhouette(kmeans_fit_stats, dists = dists)),
    names(silhouette(kmeans_fit_ClusterR, dists = dists))
  )

  expect_equal(
    silhouette_avg_vec(kmeans_fit_stats, dists = dists),
    0.4993742,
    tolerance = 0.005
  )
  expect_equal(
    silhouette_avg_vec(kmeans_fit_ClusterR, dists = dists),
    0.5473414,
    tolerance = 0.005
  )
})

test_that("kmeans sihouette metrics work with new data", {
  kmeans_fit_stats <- k_means(num_clusters = mtcars[1:3, ]) |>
    set_engine("stats", algorithm = "MacQueen") |>
    fit(~., mtcars)

  kmeans_fit_ClusterR <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., mtcars)

  new_data <- mtcars[1:4, ]

  expect_equal(
    names(silhouette(kmeans_fit_stats, new_data = new_data)),
    names(silhouette(kmeans_fit_ClusterR, new_data = new_data))
  )

  expect_equal(
    silhouette_avg_vec(kmeans_fit_stats, new_data = new_data),
    0.5176315,
    tolerance = 0.005
  )
  expect_equal(
    silhouette_avg_vec(kmeans_fit_ClusterR, new_data = new_data),
    0.5176315,
    tolerance = 0.005
  )
})
