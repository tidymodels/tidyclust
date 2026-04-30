test_that("axe_env removes terms environment", {
  skip_if_not_installed("butcher")

  k_fit <- k_means(num_clusters = 3) |>
    parsnip::set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::axe_env(k_fit)

  expect_identical(
    environmentName(attr(k_axed$preproc$terms, ".Environment")),
    "base"
  )
  expect_s3_class(k_axed, "cluster_fit")
})

test_that("axe_data removes training_data attribute", {
  skip_if_not_installed("butcher")

  k_fit <- k_means(num_clusters = 3) |>
    parsnip::set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::axe_data(k_fit)

  expect_null(attr(k_axed$fit, "training_data"))
  expect_s3_class(k_axed, "cluster_fit")
})

test_that("predict still works after butchering", {
  skip_if_not_installed("butcher")

  k_fit <- k_means(num_clusters = 3) |>
    parsnip::set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::butcher(k_fit)

  expect_equal(nrow(predict(k_axed, mtcars)), 32)
})

test_that("butchering reduces serialized size", {
  skip_if_not_installed("butcher")

  big_data <- data.frame(matrix(rnorm(1e4), ncol = 10))
  k_fit <- k_means(num_clusters = 3) |>
    parsnip::set_engine("stats") |>
    fit(~., data = big_data)

  k_axed <- butcher::axe_data(k_fit)

  f1 <- tempfile()
  f2 <- tempfile()
  on.exit(unlink(c(f1, f2)))

  saveRDS(k_fit, f1)
  saveRDS(k_axed, f2)

  expect_lt(file.size(f2), file.size(f1))
})
