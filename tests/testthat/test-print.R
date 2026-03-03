test_that("print.cluster_spec() works", {
  expect_snapshot(
    print(k_means(num_clusters = 3))
  )
})

test_that("print.cluster_spec() works with engine set", {
  expect_snapshot(
    print(k_means(num_clusters = 3) |> set_engine("stats"))
  )
})

test_that("print.cluster_fit() works", {
  set.seed(1234)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_snapshot(
    print(fit)
  )
})

test_that("print.cluster_fit() works for hier_clust", {
  fit <- hier_clust(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_snapshot(
    print(fit)
  )
})
