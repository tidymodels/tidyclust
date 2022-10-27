test_that("control class", {
  x <- k_means(num_clusters = 5) %>% set_engine("stats")

  expect_no_error(
    fit(x, ~ ., data = mtcars, control = parsnip::control_parsnip())
  )
  skip("waiting for workflow PR")
  ctrl <- control_cluster()
  class(ctrl) <- c("potato", "chair")
  expect_snapshot(
    error = TRUE,
    fit(x, mpg ~ ., data = mtcars, control = ctrl)
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(x, x = mtcars[, -1], y = mtcars$mpg, control = ctrl)
  )
})
