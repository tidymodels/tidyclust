test_that("tidy() returns expected columns for k_means", {
  skip_if_not_installed("broom")

  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- tidy(kmeans_fit)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_named(
    res,
    c(names(mtcars), "size", "withinss", "cluster"),
    ignore.order = TRUE
  )
})

test_that("glance() returns single row summary", {
  skip_if_not_installed("broom")

  kmeans_fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  res <- glance(kmeans_fit)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_named(res, c("totss", "tot.withinss", "betweenss", "iter"))
})
