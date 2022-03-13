test_that("Right classes", {
  expect_equal(class(k_means()), c("k_means", "cluster_spec"))
})

test_that("printing", {
  expect_snapshot(
    k_means()
  )
  expect_snapshot(
    k_means(k = 10)
  )
})
