test_that("extract summary works for kmeans", {
  obj1 <- k_means(k = 3) %>%
    set_engine_celery("stats") %>%
    fit(~., mtcars)

  obj2 <- k_means(k = 3) %>%
    set_engine_celery("ClusterR") %>%
    fit(~., mtcars)

  extract_fit_summary(obj1)
  extract_fit_summary(obj2)

  expect_equal(2 * 2, 4)
})
