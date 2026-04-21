test_that("sse_within() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_within(spec)
  )
})

test_that("sse_within_total() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_within_total(spec)
  )
})

test_that("sse_total() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_total(spec)
  )
})

test_that("sse_ratio() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_ratio(spec)
  )
})

# Positive tests for SSE metrics

test_that("sse_within() returns expected structure", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- sse_within(kmeans_fit)

  expect_named(res, c(".cluster", "wss", "n_members"))
  expect_equal(nrow(res), 3)
})

test_that("sse_within_total() returns expected structure", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- sse_within_total(kmeans_fit)

  expected <- tibble::tibble(
    .metric = "sse_within_total",
    .estimator = "standard",
    .estimate = sum(sse_within(kmeans_fit)$wss)
  )

  expect_equal(res, expected)
})

test_that("sse_within_total() equals sum of sse_within()", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  within <- sse_within(kmeans_fit)
  within_total <- sse_within_total(kmeans_fit)

  expect_equal(sum(within$wss), within_total$.estimate)
})

test_that("sse_within_total() respects dist_fun when new_data is NULL", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  default_res <- sse_within_total(kmeans_fit)
  custom_res <- sse_within_total(
    kmeans_fit,
    dist_fun = function(x, y) philentropy::dist_many_many(x, y, method = "manhattan")
  )

  expect_false(isTRUE(all.equal(default_res$.estimate, custom_res$.estimate)))
})

test_that("sse_total() returns expected structure", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- sse_total(kmeans_fit)

  expected <- tibble::tibble(
    .metric = "sse_total",
    .estimator = "standard",
    .estimate = sse_total_vec(kmeans_fit)
  )

  expect_equal(res, expected)
})

test_that("sse_ratio() is between 0 and 1", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- sse_ratio(kmeans_fit)

  expected <- tibble::tibble(
    .metric = "sse_ratio",
    .estimator = "standard",
    .estimate = sse_ratio_vec(kmeans_fit)
  )

  expect_equal(res, expected)
  expect_gte(res$.estimate, 0)
  expect_lte(res$.estimate, 1)
})

test_that("sse_ratio() equals sse_within_total() / sse_total()", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  ratio <- sse_ratio(kmeans_fit)
  within_total <- sse_within_total(kmeans_fit)
  total <- sse_total(kmeans_fit)

  expect_equal(ratio$.estimate, within_total$.estimate / total$.estimate)
})

test_that("sse_within_total_vec() returns numeric value", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_type(sse_within_total_vec(kmeans_fit), "double")
})

test_that("sse_total_vec() returns numeric value", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_type(sse_total_vec(kmeans_fit), "double")
})

test_that("sse_ratio_vec() returns numeric value between 0 and 1", {
  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- sse_ratio_vec(kmeans_fit)

  expect_type(res, "double")
  expect_gte(res, 0)
  expect_lte(res, 1)
})
