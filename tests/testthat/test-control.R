test_that("control class", {
  x <- k_means(num_clusters = 5) |> set_engine("stats")

  expect_no_error(
    fit(x, ~., data = mtcars, control = parsnip::control_parsnip())
  )

  ctrl <- control_cluster()
  class(ctrl) <- c("potato", "chair")
  expect_no_error(
    fit(x, ~., data = mtcars, control = ctrl)
  )
  expect_no_error(
    fit_xy(x, x = mtcars[, -1], control = ctrl)
  )
})

test_that("control_cluster() error with wrong input", {
  expect_snapshot(
    error = TRUE,
    control_cluster(verbosity = 5.5)
  )

  expect_snapshot(
    error = TRUE,
    control_cluster(verbosity = "3")
  )

  expect_snapshot(
    error = TRUE,
    control_cluster(catch = "yes")
  )
})
