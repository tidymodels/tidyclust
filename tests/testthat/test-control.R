test_that("control class", {
  x <- k_means(k = 5) %>% set_engine_celery("stats")
  ctrl <- control_celery()
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
