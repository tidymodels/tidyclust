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

test_that("print.cluster_fit() shows elapsed time when verbosity > 1", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |> set_engine("stats")
  ctrl <- control_cluster(verbosity = 2)
  fit <- fit(spec, ~., data = mtcars, control = ctrl)

  scrub_time <- function(x) gsub("Fit time: .*", "Fit time: <scrubbed>", x)
  expect_snapshot(print(fit), transform = scrub_time)
})

test_that("print.cluster_fit() handles try-error", {
  set.seed(1234)
  spec <- k_means(num_clusters = 3) |> set_engine("stats")
  fit <- fit(spec, ~., data = mtcars)
  fit$fit <- try(stop("intentional error for testing"), silent = TRUE)

  expect_snapshot(print(fit))
})

test_that("print.cluster_spec() works with translated spec", {
  spec <- k_means(num_clusters = 3) |> set_engine("stats")
  spec <- translate_tidyclust(spec)

  expect_snapshot(print(spec))
})
