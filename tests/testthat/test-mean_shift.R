test_that("primary arguments", {
  basic <- mean_shift(mode = "partition")
  basic_LPCM <- translate_tidyclust(basic |> set_engine("LPCM"))
  expect_equal(
    basic_LPCM$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      bandwidth = rlang::expr(missing_arg())
    )
  )

  ms <- mean_shift(bandwidth = 0.5, mode = "partition")
  ms_LPCM <- translate_tidyclust(ms |> set_engine("LPCM"))
  expect_equal(
    ms_LPCM$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      bandwidth = rlang::expr(missing_arg()),
      bandwidth = new_empty_quosure(0.5)
    )
  )
})

test_that("bad input", {
  expect_snapshot(error = TRUE, mean_shift(mode = "bogus"))

  skip_if_not_installed("LPCM")

  expect_snapshot(error = TRUE, {
    bt <- mean_shift(bandwidth = -1) |> set_engine("LPCM")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(mean_shift(), engine = NULL)
  )
  expect_snapshot(error = TRUE, translate_tidyclust(mean_shift(formula = ~x)))
})

test_that("predictions", {
  skip_if_not_installed("LPCM")
  set.seed(1234)
  ms_fit <- mean_shift(bandwidth = 0.5) |>
    set_engine("LPCM") |>
    fit(~., mtcars)

  preds <- predict(ms_fit, mtcars)
  expect_s3_class(preds$.pred_cluster, "factor")
  expect_identical(nrow(preds), nrow(mtcars))

  assignments <- extract_cluster_assignment(ms_fit)
  expect_identical(
    levels(preds$.pred_cluster),
    levels(assignments$.cluster)
  )
})

test_that("extract_centroids work", {
  skip_if_not_installed("LPCM")
  set.seed(1234)
  ms_fit <- mean_shift(bandwidth = 0.5) |>
    set_engine("LPCM") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- LPCM::ms(as.matrix(mtcars), h = 0.5, plot = FALSE)
  ref_centroids <- sweep(ref_res$cluster.center, 2, ref_res$scaled.by, "*")
  ref_centroids <- tibble::as_tibble(ref_centroids, .name_repair = "minimal")
  colnames(ref_centroids) <- colnames(mtcars)

  expect_equal(
    extract_centroids(ms_fit) |> dplyr::select(-.cluster),
    ref_centroids
  )
})

test_that("extract_cluster_assignment works", {
  skip_if_not_installed("LPCM")
  set.seed(1234)
  ms_fit <- mean_shift(bandwidth = 0.5) |>
    set_engine("LPCM") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- LPCM::ms(as.matrix(mtcars), h = 0.5, plot = FALSE)

  expect_equal(
    extract_cluster_assignment(ms_fit)$.cluster |> as.numeric(),
    ref_res$cluster.label
  )
})

test_that("Right classes", {
  expect_equal(
    class(mean_shift()),
    c("mean_shift", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    mean_shift()
  )
  expect_snapshot(
    mean_shift(bandwidth = 0.5)
  )
})

test_that("updating", {
  expect_snapshot(
    mean_shift(bandwidth = 0.5) |>
      update(bandwidth = tune())
  )
})

test_that("errors if `bandwidth` isn't specified", {
  skip_if_not_installed("LPCM")
  expect_snapshot(
    error = TRUE,
    mean_shift() |>
      set_engine("LPCM") |>
      fit(~., data = mtcars)
  )
})
