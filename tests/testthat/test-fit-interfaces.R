test_that("k_means fits with formula interface", {
  expect_no_condition(
    k_means(num_clusters = 3) |>
      set_engine("stats") |>
      fit(~., data = mtcars)
  )
})

test_that("k_means fits with xy interface", {
  expect_no_condition(
    k_means(num_clusters = 3) |>
      set_engine("stats") |>
      fit_xy(mtcars)
  )
})

test_that("hier_clust fits with formula interface", {
  expect_no_condition(
    hier_clust(num_clusters = 3) |>
      set_engine("stats") |>
      fit(~., data = mtcars)
  )
})

test_that("hier_clust fits with xy interface", {
  expect_no_condition(
    hier_clust(num_clusters = 3) |>
      set_engine("stats") |>
      fit_xy(mtcars)
  )
})

test_that("k_means with ClusterR fits with formula interface", {
  skip_if_not_installed("ClusterR")

  expect_no_condition(
    k_means(num_clusters = 3) |>
      set_engine("ClusterR") |>
      fit(~., data = mtcars)
  )
})

test_that("k_means with ClusterR fits with xy interface", {
  skip_if_not_installed("ClusterR")

  expect_no_condition(
    k_means(num_clusters = 3) |>
      set_engine("ClusterR") |>
      fit_xy(mtcars)
  )
})
