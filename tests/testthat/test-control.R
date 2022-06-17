test_that("control class", {
  skip("waiting for workflow PR")
  x <- k_means(k = 5) %>% set_engine_tidyclust("stats")
  ctrl <- control_tidyclust()
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
