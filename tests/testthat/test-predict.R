test_that("predict() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    predict(spec)
  )
})
