obj1 <- k_means(k = mtcars[1:3,]) %>%
  set_engine_celery("stats", algorithm = "MacQueen") %>%
  fit(~., mtcars)

obj2 <- k_means(k = 3) %>%
  set_engine_celery("ClusterR", CENTROIDS = as.matrix(mtcars[1:3,])) %>%
  fit(~., mtcars)


test_that("kmeans sse metrics work", {

  expect_equal(within_cluster_sse(obj1)$sse,
               c(7654.146, 76954.010, 42877.103),
               tolerance = 0.005)

  expect_equal(tot_wss(obj1), 127485.3, tolerance = 0.005)
  expect_equal(tot_sse(obj1), 623387.5, tolerance = 0.005)
  expect_equal(sse_ratio(obj1), 0.204504, tolerance = 0.005)


  expect_equal(within_cluster_sse(obj2)$sse,
               c(4665.041, 42877.103, 56041.432),
               tolerance = 0.005)

  expect_equal(tot_wss(obj2), 103583.6, tolerance = 0.005)
  expect_equal(tot_sse(obj2), tot_sse(obj1), tolerance = 0.005)
  expect_equal(sse_ratio(obj2), 0.1661624, tolerance = 0.005)

})

test_that("kmeans sihouette metrics work", {

  dists <- mtcars %>%
     as.matrix() %>%
     dist()

  c1 <- extract_fit_summary(obj1)$cluster_assignments
  c2 <- extract_fit_summary(obj2)$cluster_assignments

  expect_equal(names(silhouettes(dists, c1)),
               names(silhouettes(dists, c2)))

  expect_equal(avg_silhouette(dists, c1), 0.4993742,
               tolerance = 0.005)
  expect_equal(avg_silhouette(dists, c2), 0.5473414,
               tolerance = 0.005)

})
