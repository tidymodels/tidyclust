obj1 <- k_means(k = mtcars[1:3, ]) %>%
  set_engine_celery("stats", algorithm = "MacQueen") %>%
  fit(~., mtcars)

obj2 <- k_means(k = 3) %>%
  set_engine_celery("ClusterR", CENTROIDS = as.matrix(mtcars[1:3, ])) %>%
  fit(~., mtcars)


km_orig <- kmeans(mtcars, centers = mtcars[1:3, ], algorithm = "MacQueen")
km_orig_2 <- ClusterR::KMeans_rcpp(mtcars, clusters = 3, CENTROIDS = as.matrix(mtcars[1:3, ]))

new_data <- mtcars[1:4, ]

test_that("kmeans sse metrics work", {
  expect_equal(within_cluster_sse(obj1)$wss,
    c(42877.103, 76954.010, 7654.146), # hard coded because of order
    tolerance = 0.005
  )

  expect_equal(tot_wss(obj1), km_orig$tot.withinss, tolerance = 0.005)
  expect_equal(tot_sse(obj1), km_orig$totss, tolerance = 0.005)
  expect_equal(sse_ratio(obj1), km_orig$tot.withinss / km_orig$totss, tolerance = 0.005)

  expect_equal(within_cluster_sse(obj2)$wss,
    c(56041.432, 4665.041, 42877.103), # hard coded because of order
    tolerance = 0.005
  )

  expect_equal(tot_wss(obj2), sum(km_orig_2$WCSS_per_cluster), tolerance = 0.005)
  expect_equal(tot_sse(obj2), tot_sse(obj1), tolerance = 0.005)
  expect_equal(sse_ratio(obj2), 0.1661624, tolerance = 0.005)
})

test_that("kmeans sse metrics work on new data", {
  expect_equal(within_cluster_sse(obj1, new_data)$wss,
    c(2799.21, 12855.17),
    tolerance = 0.005
  )

  expect_equal(tot_wss(obj1, new_data), 15654.38, tolerance = 0.005)
  expect_equal(tot_sse(obj1, new_data), 32763.7, tolerance = 0.005)
  expect_equal(sse_ratio(obj1, new_data), 15654.38 / 32763.7, tolerance = 0.005)
})

test_that("kmeans sihouette metrics work", {
  dists <- mtcars %>%
    as.matrix() %>%
    dist()

  expect_equal(
    names(silhouettes(obj1, dists = dists)),
    names(silhouettes(obj2, dists = dists))
  )

  expect_equal(avg_silhouette(obj1, dists = dists), 0.4993742,
    tolerance = 0.005
  )
  expect_equal(avg_silhouette(obj2, dists = dists), 0.5473414,
    tolerance = 0.005
  )
})


test_that("kmeans sihouette metrics work with new data", {
  expect_equal(
    names(silhouettes(obj1, new_data = new_data)),
    names(silhouettes(obj2, new_data = new_data))
  )

  expect_equal(avg_silhouette(obj1, new_data = new_data), 0.5176315,
    tolerance = 0.005
  )
  expect_equal(avg_silhouette(obj2, new_data = new_data), 0.5176315,
    tolerance = 0.005
  )
})
