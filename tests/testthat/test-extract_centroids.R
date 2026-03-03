toy_df <- data.frame(
  "beer"    = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  "milk"    = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  "bread"   = c(TRUE, TRUE, FALSE, TRUE, TRUE),
  "diapers" = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  "eggs"    = c(FALSE, TRUE, FALSE, FALSE, FALSE)
)

test_that("extract_centroids() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_centroids(spec)
  )
})

test_that("extract_centroids() errors for hier_clust() with missing args", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_centroids()
  )
})

test_that("extract_centroids() errors for hier_clust() with k arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_centroids(k = 3)
  )
})

test_that("extract_centroids() errors for hier_clust() with h arg", {
  hclust_spec <- hier_clust()

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  expect_snapshot(
    error = TRUE,
    hclust_fit |>
      extract_centroids(h = 3)
  )
})

test_that("passed arguments overwrites model arguments", {
  hclust_spec <- hier_clust(num_clusters = 4)

  hclust_fit <- fit(hclust_spec, ~., mtcars)

  res <- hclust_fit |>
    extract_centroids(num_clusters = 1)

  expect_identical(length(levels(res$.cluster)), 1L)
})

test_that("prefix is passed in extract_centroids()", {
  spec <- tidyclust::k_means(num_clusters = 4) |>
    fit(~., data = mtcars)

  res <- extract_centroids(spec, prefix = "C_")

  expect_true(
    all(substr(res$.cluster, 1, 2) == "C_")
  )
})

test_that("extract_centroids errors for freq_itemsets", {
  set.seed(1234)
  skip_if_not_installed("arules")
  fi_fit <- freq_itemsets(min_support = 0.5) %>%
    set_engine("arules") %>%
    fit(~., toy_df %>% dplyr::mutate(across(everything(), as.numeric)))

  expect_snapshot(error = TRUE, extract_centroids(fi_fit))
})
