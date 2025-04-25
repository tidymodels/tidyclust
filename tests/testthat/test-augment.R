test_that("partition models", {
  x <- k_means(num_clusters = 2)

  set.seed(1234)
  reg_form <- x |> fit(~., data = mtcars)
  set.seed(1234)
  reg_xy <- x |> fit_xy(mtcars)

  expect_equal(
    colnames(augment(reg_form, head(mtcars))),
    c(
      "mpg",
      "cyl",
      "disp",
      "hp",
      "drat",
      "wt",
      "qsec",
      "vs",
      "am",
      "gear",
      "carb",
      ".pred_cluster"
    )
  )
  expect_equal(nrow(augment(reg_form, head(mtcars))), 6)

  expect_equal(
    colnames(augment(reg_xy, head(mtcars))),
    c(
      "mpg",
      "cyl",
      "disp",
      "hp",
      "drat",
      "wt",
      "qsec",
      "vs",
      "am",
      "gear",
      "carb",
      ".pred_cluster"
    )
  )
  expect_equal(nrow(augment(reg_xy, head(mtcars))), 6)

  expect_s3_class(augment(reg_form, head(mtcars)), "tbl_df")

  reg_form$spec$mode <- "depeche"

  expect_snapshot(error = TRUE, augment(reg_form, head(mtcars[, -1])))
})
