test_that("sse_within() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_sse_within(spec)
  )
})

test_that("sse_within_total() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_sse_within_total(spec)
  )
})

test_that("sse_total() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_sse_within_total(spec)
  )
})

test_that("sse_ratio() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    sse_ratio(spec)
  )
})
