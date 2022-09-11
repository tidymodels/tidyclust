test_that("fit and fit_xy errors if outcome is provided", {

  expect_error(
    k_means(num_clusters = 5) %>% fit_xy(mtcars),
    regexp = NA
  )

  expect_snapshot(error = TRUE,
    k_means(num_clusters = 5) %>%
      fit_xy(mtcars, y = mtcars$mpg)
  )

  km <- k_means(num_clusters = 5)

  expect_error(
    workflows::workflow(~., km) %>% fit(mtcars),
    regexp = NA
  )
  expect_snapshot(error = TRUE,
    workflows::workflow(mpg~., km) %>% fit(mtcars)
  )
})
