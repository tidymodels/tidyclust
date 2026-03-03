# .convert_form_to_x_fit() tests -----------------------------------------------

test_that(".convert_form_to_x_fit() errors on invalid composition", {
  expect_snapshot(
    error = TRUE,
    .convert_form_to_x_fit(~., mtcars, composition = "invalid")
  )
})

test_that(".convert_form_to_x_fit() errors on non-numeric weights", {
  expect_snapshot(
    error = TRUE,
    .convert_form_to_x_fit(~., mtcars, weights = letters[1:32])
  )
})

test_that(".convert_form_to_x_fit() errors on invalid dots arguments", {
  expect_snapshot(
    error = TRUE,
    .convert_form_to_x_fit(~., mtcars, bad_arg = 1)
  )
})

test_that(".convert_form_to_x_fit() processes subset argument", {
  res <- .convert_form_to_x_fit(~., mtcars, subset = 1:10)
  expect_equal(nrow(res$x), 10)
})

test_that(".convert_form_to_x_fit() works with one_hot indicators", {
  skip("contr_one_hot not available in tidyclust")

  data <- data.frame(x = 1:10, cat = factor(rep(c("a", "b"), 5)))
  res <- .convert_form_to_x_fit(~., data, indicators = "one_hot")

  expect_named(res$x, c("x", "cata", "catb"))
})

test_that(".convert_form_to_x_fit() works with indicators = none", {
  data <- data.frame(x = 1:10, cat = factor(rep(c("a", "b"), 5)))
  res <- .convert_form_to_x_fit(~., data, indicators = "none")

  expect_s3_class(res$x, "data.frame")
  expect_named(res$x, c("x", "cat"))
})

test_that(".convert_form_to_x_fit() returns matrix when requested", {
  res <- .convert_form_to_x_fit(~., mtcars, composition = "matrix")
  expect_true(is.matrix(res$x))
})

test_that(".convert_form_to_x_fit() returns data.frame by default", {
  res <- .convert_form_to_x_fit(~., mtcars, composition = "data.frame")
  expect_s3_class(res$x, "data.frame")
})

test_that(".convert_form_to_x_fit() removes intercept column by default", {
  res <- .convert_form_to_x_fit(~., mtcars, remove_intercept = TRUE)
  expect_false("(Intercept)" %in% colnames(res$x))
})

test_that(".convert_form_to_x_fit() keeps intercept when requested", {
  res <- .convert_form_to_x_fit(~., mtcars, remove_intercept = FALSE)
  expect_named(res$x, c("(Intercept)", names(mtcars)))
})

test_that(".convert_form_to_x_fit() returns terms object", {
  res <- .convert_form_to_x_fit(~., mtcars)
  expect_s3_class(res$terms, "terms")
})

test_that(".convert_form_to_x_fit() returns xlevels for factors", {
  data <- data.frame(x = 1:10, cat = factor(rep(c("a", "b"), 5)))
  res <- .convert_form_to_x_fit(~., data)
  expect_named(res$xlevels, "cat")
})

test_that(".convert_form_to_x_fit() accepts valid weights", {
  res <- .convert_form_to_x_fit(~., mtcars, weights = rep(1, 32))
  expect_equal(res$weights, rep(1, 32))
})

test_that(".convert_form_to_x_fit() stores options", {
  res <- .convert_form_to_x_fit(
    ~.,
    mtcars,
    indicators = "traditional",
    composition = "matrix",
    remove_intercept = FALSE
  )
  expect_equal(res$options$indicators, "traditional")
  expect_equal(res$options$composition, "matrix")
  expect_false(res$options$remove_intercept)
})

# .convert_form_to_x_new() tests -----------------------------------------------

test_that(".convert_form_to_x_new() errors on invalid composition", {
  fit <- k_means(num_clusters = 3) |> set_engine("stats") |> fit(~., mtcars)

  expect_snapshot(
    error = TRUE,
    .convert_form_to_x_new(fit$preproc, mtcars, composition = "invalid")
  )
})

test_that(".convert_form_to_x_new() works with matrix composition", {
  fit <- k_means(num_clusters = 3) |> set_engine("stats") |> fit(~., mtcars)

  res <- .convert_form_to_x_new(
    fit$preproc,
    mtcars[1:5, ],
    composition = "matrix"
  )
  expect_true(is.matrix(res$x))
  expect_equal(nrow(res$x), 5)
})

test_that(".convert_form_to_x_new() works with data.frame composition", {
  fit <- k_means(num_clusters = 3) |> set_engine("stats") |> fit(~., mtcars)

  res <- .convert_form_to_x_new(
    fit$preproc,
    mtcars[1:5, ],
    composition = "data.frame"
  )
  expect_s3_class(res$x, "data.frame")
})

test_that(".convert_form_to_x_new() works with one_hot indicators", {
  skip("contr_one_hot not available in tidyclust")

  data <- data.frame(x = 1:10, cat = factor(rep(c("a", "b"), 5)))

  preproc <- .convert_form_to_x_fit(~., data, indicators = "one_hot")
  res <- .convert_form_to_x_new(preproc, data[1:3, ])

  expect_named(res$x, c("x", "cata", "catb"))
})

# .convert_x_to_form_fit() tests -----------------------------------------------

test_that(".convert_x_to_form_fit() errors when x is a vector", {
  expect_snapshot(
    error = TRUE,
    .convert_x_to_form_fit(1:10)
  )
})

test_that(".convert_x_to_form_fit() errors on non-numeric weights", {
  expect_snapshot(
    error = TRUE,
    .convert_x_to_form_fit(mtcars, weights = letters[1:32])
  )
})
test_that(".convert_x_to_form_fit() errors on wrong length weights", {
  expect_snapshot(
    error = TRUE,
    .convert_x_to_form_fit(mtcars, weights = 1:5)
  )
})

test_that(".convert_x_to_form_fit() works with matrix input", {
  mat <- as.matrix(mtcars)
  res <- .convert_x_to_form_fit(mat)

  expect_s3_class(res$formula, "formula")
  expect_s3_class(res$data, "data.frame")
  expect_equal(res$x_var, colnames(mtcars))
})

test_that(".convert_x_to_form_fit() works with data.frame input", {
  res <- .convert_x_to_form_fit(mtcars)

  expect_s3_class(res$formula, "formula")
  expect_s3_class(res$data, "data.frame")
  expect_equal(res$x_var, names(mtcars))
})

test_that(".convert_x_to_form_fit() removes intercept column", {
  mat <- cbind(`(Intercept)` = 1, as.matrix(mtcars))
  res <- .convert_x_to_form_fit(mat, remove_intercept = TRUE)

  expect_false("(Intercept)" %in% names(res$data))
})

test_that(".convert_x_to_form_fit() preserves row names", {
  res <- .convert_x_to_form_fit(mtcars)
  expect_equal(rownames(res$data), rownames(mtcars))
})

test_that(".convert_x_to_form_fit() accepts valid weights", {
  res <- .convert_x_to_form_fit(mtcars, weights = rep(1, 32))
  expect_equal(res$weights, rep(1, 32))
})

# .convert_x_to_form_new() tests -----------------------------------------------

test_that(".convert_x_to_form_new() subsets to expected columns", {
  preproc <- list(x_var = c("mpg", "cyl", "disp"))
  res <- .convert_x_to_form_new(preproc, mtcars)

  expect_named(res, c("mpg", "cyl", "disp"))
})

test_that(".convert_x_to_form_new() converts matrix to data.frame", {
  preproc <- list(x_var = c("mpg", "cyl", "disp"))
  mat <- as.matrix(mtcars)
  res <- .convert_x_to_form_new(preproc, mat)

  expect_s3_class(res, "data.frame")
})

# make_formula() tests ---------------------------------------------------------

test_that("make_formula() creates short formula by default", {
  res <- make_formula(c("a", "b", "c"))
  expect_equal(as.character(res), c("~", "."))
})

test_that("make_formula() creates long formula when short = FALSE", {
  res <- make_formula(c("a", "b", "c"), short = FALSE)
  expect_equal(as.character(res)[2], "a + b + c")
})
