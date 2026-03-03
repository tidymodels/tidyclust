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

test_that("predict with type = 'raw' errors when not available", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_snapshot(
    error = TRUE,
    predict(fit, mtcars, type = "raw")
  )
})

test_that("predict errors with NA in new_data", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  new_data <- mtcars[1:3, ]
  new_data[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    predict(fit, new_data)
  )
})

test_that("predict works with different column order", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  reordered <- mtcars[, rev(names(mtcars))]
  original <- predict(fit, mtcars)
  res <- predict(fit, reordered)

  expect_identical(res, original)
})

test_that("predict ignores extra columns", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  extra_cols <- mtcars
  extra_cols$extra <- seq_len(nrow(mtcars))
  original <- predict(fit, mtcars)
  res <- predict(fit, extra_cols)

  expect_identical(res, original)
})

test_that("predict errors with missing required columns", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  missing_cols <- mtcars[, 1:5]

  expect_snapshot(
    error = TRUE,
    predict(fit, missing_cols)
  )
})
