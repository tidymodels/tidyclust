test_that("extract_cluster_assignment() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_cluster_assignment(spec)
  )
})

test_that("extract_cluster_assignment() errors for hier_clust() with missing args", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_cluster_assignment()
  )
})

test_that("extract_cluster_assignment() errors for hier_clust() with k arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_cluster_assignment(k = 3)
  )
})

test_that("extract_cluster_assignment() errors for hier_clust() with h arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_cluster_assignment(h = 3)
  )
})

test_that("passed arguments overwrites model arguments", {
  hclust_spec <- hier_clust(num_clusters = 4)

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  res <- hclust_fit |>
    extract_cluster_assignment(num_clusters = 1)

  expect_identical(length(levels(res$.cluster)), 1L)
})

test_that("prefix is passed in extract_cluster_assignment()", {
  spec <- tidyclust::k_means(num_clusters = 4) |>
    fit(~., data = mtcars)

  res <- extract_cluster_assignment(spec, prefix = "C_")

  expect_true(
    all(substr(res$.cluster, 1, 2) == "C_")
  )
})
