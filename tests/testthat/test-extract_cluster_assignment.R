test_that("extract_cluster_assignment() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_cluster_assignment(spec)
  )
})
