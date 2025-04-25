test_that("primary arguments", {
  basic <- k_means(mode = "partition")
  basic_stats <- translate_tidyclust(basic |> set_engine("stats"))
  expect_equal(
    basic_stats$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      centers = rlang::expr(missing_arg())
    )
  )

  k <- k_means(num_clusters = 15, mode = "partition")
  k_stats <- translate_tidyclust(k |> set_engine("stats"))
  expect_equal(
    k_stats$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      centers = rlang::expr(missing_arg()),
      centers = new_empty_quosure(15)
    )
  )
})

test_that("engine arguments", {
  stats_print <- k_means(mode = "partition")
  expect_equal(
    translate_tidyclust(
      stats_print |>
        set_engine("stats", nstart = 1L)
    )$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      centers = rlang::expr(missing_arg()),
      nstart = new_empty_quosure(1L)
    )
  )
})

test_that("bad input", {
  expect_snapshot(error = TRUE, k_means(mode = "bogus"))
  expect_snapshot(error = TRUE, {
    bt <- k_means(num_clusters = -1) |> set_engine("stats")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, translate_tidyclust(k_means(), engine = NULL))
  expect_snapshot(error = TRUE, translate_tidyclust(k_means(formula = ~x)))
})

test_that("predictions", {
  set.seed(1234)
  kmeans_fit <- k_means(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- kmeans(mtcars, 4)

  ref_predictions <- ref_res$centers |>
    flexclust::dist2(mtcars) |>
    apply(2, which.min) |>
    unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) |> as.numeric()
  }

  expect_equal(
    relevel_preds(ref_predictions),
    predict(kmeans_fit, mtcars)$.pred_cluster |> as.numeric()
  )

  expect_equal(
    relevel_preds(ref_predictions),
    extract_cluster_assignment(kmeans_fit)$.cluster |> as.numeric()
  )
})

test_that("extract_centroids work", {
  set.seed(1234)
  kmeans_fit <- k_means(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- kmeans(mtcars, 4)

  ref_centroids <- dplyr::bind_cols(
    .cluster = c(1L, 4L, 2L, 3L),
    ref_res$centers
  ) |>
    dplyr::arrange(.cluster) |>
    dplyr::select(-.cluster)

  expect_identical(
    extract_centroids(kmeans_fit) |> dplyr::select(-.cluster),
    ref_centroids
  )
})

test_that("Right classes", {
  expect_equal(
    class(k_means()),
    c("k_means", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    k_means()
  )
  expect_snapshot(
    k_means(num_clusters = 10)
  )
})

test_that("updating", {
  expect_snapshot(
    k_means(num_clusters = 5) |>
      update(num_clusters = tune())
  )
})

test_that("Engine-specific arguments are passed to ClusterR models", {
  spec <- k_means(num_clusters = 2) |>
    set_engine("ClusterR", fuzzy = FALSE)

  fit <- fit(spec, ~., data = mtcars)
  expect_true(is.null(fit$fit$fuzzy_clusters))

  spec <- k_means(num_clusters = 2) |>
    set_engine("ClusterR", fuzzy = TRUE)

  fit <- fit(spec, ~., data = mtcars)
  expect_false(is.null(fit$fit$fuzzy_clusters))
})

test_that("reordering is done correctly for stats k_means", {
  set.seed(42)

  kmeans_fit <- k_means(num_clusters = 6) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  summ <- extract_fit_summary(kmeans_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})

test_that("reordering is done correctly for ClusterR k_means", {
  set.seed(42)

  kmeans_fit <- k_means(num_clusters = 6) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  summ <- extract_fit_summary(kmeans_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})

test_that("errors if `num_clust` isn't specified", {
  expect_snapshot(
    error = TRUE,
    k_means() |>
      set_engine("stats") |>
      fit(~., data = mtcars)
  )

  expect_snapshot(
    error = TRUE,
    k_means() |>
      set_engine("ClusterR") |>
      fit(~., data = mtcars)
  )
})
