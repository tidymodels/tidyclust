test_that("extract_centroids() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_centroids(spec)
  )
})

test_that("prefix is passed in extract_centroids()", {
  spec <- tidyclust::k_means(num_clusters = 4) %>%
    fit(~ ., data = mtcars)

  res <- extract_centroids(spec, prefix = "C_")

  expect_true(
    all(substr(res$.cluster, 1, 2) == "C_")
  )
})
