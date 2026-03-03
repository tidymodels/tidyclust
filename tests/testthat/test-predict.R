test_that("predict() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    predict(spec)
  )
})

test_that("predict() errors for hier_clust() with missing args", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      predict(mtcars)
  )
})

test_that("predict() errors for hier_clust() with k arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      predict(mtcars, k = 3)
  )
})

test_that("predict() errors for hier_clust() with h arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      predict(mtcars, h = 3)
  )
})

test_that("passed arguments overwrites model arguments", {
  hclust_spec <- hier_clust(num_clusters = 4)

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  res <- hclust_fit |>
    predict(mtcars, num_clusters = 1)

  expect_identical(length(levels(res$.pred_cluster)), 1L)
})

test_that("prefix is passed in predict()", {
  spec <- tidyclust::k_means(num_clusters = 4) |>
    fit(~., data = mtcars)

  res <- predict(spec, mtcars, prefix = "C_")

  expect_true(
    all(substr(res$.pred_cluster, 1, 2) == "C_")
  )
})

test_that("predict() errors for cluster spec for freq_itemsets", {
  skip_if_not_installed("arules")
  spec <- tidyclust::freq_itemsets(min_support = 0.5)

  expect_snapshot(
    error = TRUE,
    predict(spec)
  )
})
