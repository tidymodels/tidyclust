test_that("required_pkgs works for k_means with stats engine", {
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  expect_identical(required_pkgs(spec), c("tidyclust", "stats"))
})

test_that("required_pkgs works for k_means with ClusterR engine", {
  skip_if_not_installed("ClusterR")

  spec <- k_means(num_clusters = 3) |>
    set_engine("ClusterR")

  expect_identical(required_pkgs(spec), c("tidyclust", "ClusterR"))
})

test_that("required_pkgs works for hier_clust with stats engine", {
  spec <- hier_clust(num_clusters = 3) |>
    set_engine("stats")

  expect_identical(required_pkgs(spec), c("tidyclust", "stats"))
})

test_that("required_pkgs works for fitted models", {
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  expect_identical(required_pkgs(fit), c("tidyclust", "stats"))
})

test_that("required_pkgs works with infra = FALSE", {
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  expect_identical(required_pkgs(spec, infra = FALSE), c("stats", "tidyclust"))
})
