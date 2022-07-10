test_that('pipe arguments', {
  mod_1 <- k_means() %>%
    set_args_tidyclust(k = 1)
  expect_equal(
    rlang::quo_get_expr(mod_1$args$k),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_1$args$k),
    rlang::empty_env()
  )

  mod_2 <- k_means(k = 2) %>%
    set_args_tidyclust(k = 1)

  var_env <- rlang::current_env()

  expect_equal(
    rlang::quo_get_expr(mod_2$args$k),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_2$args$k),
    rlang::empty_env()
  )

  expect_snapshot(error = TRUE, k_means() %>% set_args_tidyclust())
})


test_that('pipe engine', {
  mod_1 <- k_means() %>%
    set_mode_tidyclust("partition")
  expect_equal(mod_1$mode, "partition")

  expect_snapshot(error = TRUE, k_means() %>% set_mode_tidyclust())
  expect_snapshot(error = TRUE, k_means() %>% set_mode_tidyclust(2))
  expect_snapshot(error = TRUE, k_means() %>% set_mode_tidyclust("haberdashery"))
})

test_that("can't set a mode that isn't allowed by the model spec", {
  expect_snapshot(error = TRUE,
    set_mode_tidyclust(k_means(), "classification")
  )
})
