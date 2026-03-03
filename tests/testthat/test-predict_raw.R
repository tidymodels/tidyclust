test_that("predict_raw() warns for try-error fit", {
  spec <- k_means(num_clusters = 3) |> set_engine("stats")
  fit <- fit(spec, ~., data = mtcars)
  fit$fit <- try(stop("intentional error"), silent = TRUE)

  # Manually set up the method to allow raw predictions for testing

  fit$spec$method$pred$raw <- list(
    args = list(object = rlang::expr(object$fit)),
    func = c(fun = "predict")
  )

  expect_snapshot(res <- predict_raw(fit, mtcars[1:5, ]))
  expect_null(res)
})
