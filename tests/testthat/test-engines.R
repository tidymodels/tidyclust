test_that("set_engine() errors with no engine set", {
  expect_snapshot(
    error = TRUE,
    set_engine(k_means())
  )
})
