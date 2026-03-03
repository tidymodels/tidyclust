test_that("primary arguments", {
  basic <- hier_clust(mode = "partition")
  basic_stats <- translate_tidyclust(basic |> set_engine("stats"))
  expect_equal(
    basic_stats$method$fit$args,
    list(
      data = rlang::expr(missing_arg()),
      linkage_method = new_empty_quosure("complete")
    )
  )
})

test_that("engine arguments", {
  stats_print <- hier_clust(mode = "partition")
  expect_equal(
    translate_tidyclust(
      stats_print |>
        set_engine("stats", members = NULL)
    )$method$fit$args,
    list(
      data = rlang::expr(missing_arg()),
      linkage_method = new_empty_quosure("complete"),
      members = new_empty_quosure(NULL)
    )
  )
})

test_that("bad input", {
  expect_snapshot(
    error = TRUE,
    hier_clust(mode = "bogus")
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- hier_clust(linkage_method = "bogus") |> set_engine("stats")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(hier_clust(), engine = NULL)
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(hier_clust(formula = ~x))
  )
})

test_that("predictions", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res |> unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) |> as.numeric()
  }

  expect_equal(
    relevel_preds(predict(hclust_fit, mtcars)$.pred_cluster),
    predict(hclust_fit, mtcars)$.pred_cluster |> as.numeric()
  )

  expect_equal(
    relevel_preds(ref_predictions),
    extract_cluster_assignment(hclust_fit)$.cluster |> as.numeric()
  )
})

test_that("extract_cluster_assignment() works if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  hclust_fit_no_args <- hier_clust() |>
    set_engine("stats") |>
    fit(~., mtcars)

  expect_identical(
    extract_cluster_assignment(hclust_fit, mtcars),
    extract_cluster_assignment(hclust_fit_no_args, mtcars, num_clusters = 4)
  )
})

test_that("predict() works if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  hclust_fit_no_args <- hier_clust() |>
    set_engine("stats") |>
    fit(~., mtcars)

  expect_identical(
    predict(hclust_fit, mtcars),
    predict(hclust_fit_no_args, mtcars, num_clusters = 4)
  )
})

test_that("extract_centroids() work", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res |> unname()

  expect_identical(
    extract_centroids(hclust_fit) |>
      dplyr::mutate(.cluster = as.integer(.cluster)),
    mtcars |>
      dplyr::group_by(.cluster = ref_predictions) |>
      dplyr::summarize(dplyr::across(dplyr::everything(), mean))
  )
})

test_that("extract_centroids() work if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust() |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res |> unname()

  expect_identical(
    extract_centroids(hclust_fit, num_clusters = 4) |>
      dplyr::mutate(.cluster = as.integer(.cluster)),
    mtcars |>
      dplyr::group_by(.cluster = ref_predictions) |>
      dplyr::summarize(dplyr::across(dplyr::everything(), mean))
  )
})

test_that("predictions with new data", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res |> unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) |> as.numeric()
  }

  expect_equal(
    relevel_preds(predict(hclust_fit, mtcars[1:10, ])$.pred_cluster),
    predict(hclust_fit, mtcars[1:10, ])$.pred_cluster |> as.numeric()
  )
})

test_that("Right classes", {
  expect_equal(
    class(hier_clust()),
    c("hier_clust", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    hier_clust()
  )
  expect_snapshot(
    hier_clust(num_clusters = 10)
  )
})

test_that("updating", {
  expect_snapshot(
    hier_clust(num_clusters = 5) |>
      update(num_clusters = tune())
  )
})

test_that("reordering is done correctly for stats hier_clust", {
  set.seed(42)

  kmeans_fit <- hier_clust(num_clusters = 6) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  summ <- extract_fit_summary(kmeans_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})

test_that("prediction works with single linkage", {
  fit <- hier_clust(num_clusters = 3, linkage_method = "single") |>
    set_engine("stats") |>
    fit(~., mtcars)

  res <- predict(fit, mtcars[1:5, ])

  expect_s3_class(res$.pred_cluster, "factor")
  expect_equal(nrow(res), 5)
})

test_that("prediction works with complete linkage", {
  fit <- hier_clust(num_clusters = 3, linkage_method = "complete") |>
    set_engine("stats") |>
    fit(~., mtcars)

  res <- predict(fit, mtcars[1:5, ])

  expect_s3_class(res$.pred_cluster, "factor")
  expect_equal(nrow(res), 5)
})

test_that("prediction works with average linkage", {
  fit <- hier_clust(num_clusters = 3, linkage_method = "average") |>
    set_engine("stats") |>
    fit(~., mtcars)

  res <- predict(fit, mtcars[1:5, ])

  expect_s3_class(res$.pred_cluster, "factor")
  expect_equal(nrow(res), 5)
})

test_that("prediction works with ward.D linkage", {
  fit <- hier_clust(num_clusters = 3, linkage_method = "ward.D") |>
    set_engine("stats") |>
    fit(~., mtcars)

  res <- predict(fit, mtcars[1:5, ])

  expect_s3_class(res$.pred_cluster, "factor")
  expect_equal(nrow(res), 5)
})

test_that("different linkage methods can produce different predictions", {
  fit_single <- hier_clust(num_clusters = 3, linkage_method = "single") |>
    set_engine("stats") |>
    fit(~., mtcars)

  fit_ward <- hier_clust(num_clusters = 3, linkage_method = "ward.D") |>
    set_engine("stats") |>
    fit(~., mtcars)

  preds_single <- predict(fit_single, mtcars)
  preds_ward <- predict(fit_ward, mtcars)

  expect_false(identical(preds_single, preds_ward))
})

test_that("num_clusters = 2 produces exactly 2 clusters", {
  fit <- hier_clust(num_clusters = 2) |>
    set_engine("stats") |>
    fit(~., mtcars)

  assignments <- extract_cluster_assignment(fit)

  expect_identical(length(unique(assignments$.cluster)), 2L)
})

test_that("num_clusters = 4 produces exactly 4 clusters", {
  fit <- hier_clust(num_clusters = 4) |>
    set_engine("stats") |>
    fit(~., mtcars)

  assignments <- extract_cluster_assignment(fit)

  expect_identical(length(unique(assignments$.cluster)), 4L)
})

test_that("num_clusters = 6 produces exactly 6 clusters", {
  fit <- hier_clust(num_clusters = 6) |>
    set_engine("stats") |>
    fit(~., mtcars)

  assignments <- extract_cluster_assignment(fit)

  expect_identical(length(unique(assignments$.cluster)), 6L)
})

test_that("cut_height matches stats::cutree", {
  fit <- hier_clust(cut_height = 300) |>
    set_engine("stats") |>
    fit(~., mtcars)

  ref <- stats::cutree(stats::hclust(dist(mtcars)), h = 300)

  assignments <- extract_cluster_assignment(fit)

  expect_identical(
    length(unique(assignments$.cluster)),
    length(unique(ref))
  )
})
