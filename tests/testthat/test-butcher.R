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

test_that("predict still works after butcher::butcher()", {
  skip_if_not_installed("butcher")

  k_fit <- k_means(num_clusters = 3) |>
    parsnip::set_engine("stats") |>
    fit(~., data = mtcars)

  k_axed <- butcher::butcher(k_fit)

  expect_equal(nrow(predict(k_axed, mtcars)), 32)
})
