test_that("extract_centroids() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_centroids(spec)
  )
})
