test_that("param ranges", {
  expect_equal(cut_height()$range$lower, 0L)
  expect_equal(cut_height(range = c(10L, 100L))$range$lower, 10L)
  expect_equal(cut_height(range = c(10L, 100L))$range$upper, 100L)
})

test_that("param values", {
  expect_equal(linkage_method()$values, values_linkage_method)
  expect_equal(
    linkage_method(values = c("single", "complete"))$values,
    c("single", "complete")
  )
})
