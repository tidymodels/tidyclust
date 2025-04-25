test_that("pipe arguments", {
  mod_1 <- k_means() |>
    set_args(num_clusters = 1)
  expect_equal(
    rlang::quo_get_expr(mod_1$args$num_clusters),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_1$args$num_clusters),
    rlang::empty_env()
  )

  mod_2 <- k_means(num_clusters = 2) |>
    set_args(num_clusters = 1)

  var_env <- rlang::current_env()

  expect_equal(
    rlang::quo_get_expr(mod_2$args$num_clusters),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_2$args$num_clusters),
    rlang::empty_env()
  )

  expect_snapshot(error = TRUE, k_means() |> set_args())
})

test_that("pipe engine", {
  mod_1 <- k_means() |>
    set_mode("partition")
  expect_equal(mod_1$mode, "partition")

  expect_snapshot(error = TRUE, k_means() |> set_mode())
  expect_snapshot(error = TRUE, k_means() |> set_mode(2))
  expect_snapshot(error = TRUE, k_means() |> set_mode("haberdashery"))
})

test_that("can't set a mode that isn't allowed by the model spec", {
  expect_snapshot(
    error = TRUE,
    set_mode(k_means(), "classification")
  )
})
