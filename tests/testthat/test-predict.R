test_that("predict() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    predict(spec)
  )
})

test_that("prefix is passed in predict()", {
  spec <- tidyclust::k_means(num_clusters = 4) %>%
    fit(~ ., data = mtcars)

  res <- predict(spec, mtcars, prefix = "C_")

  expect_true(
    all(substr(res$.pred_cluster, 1, 2) == "C_")
  )
})
