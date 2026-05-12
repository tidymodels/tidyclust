test_that("primary arguments", {
  basic <- db_clust(mode = "partition")
  basic_dbscan <- translate_tidyclust(basic %>% set_engine("dbscan"))
  expect_equal(
    basic_dbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      radius = rlang::expr(missing_arg()),
      min_points = rlang::expr(missing_arg())
    )
  )

  db <- db_clust(radius = 2, min_points = 4, mode = "partition")
  db_dbscan <- translate_tidyclust(db %>% set_engine("dbscan"))
  expect_equal(
    db_dbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      radius = rlang::expr(missing_arg()),
      min_points = rlang::expr(missing_arg()),
      radius = new_empty_quosure(2),
      min_points = new_empty_quosure(4)
    )
  )

  basic_hdbscan <- translate_tidyclust(basic |> set_engine("hdbscan"))
  expect_equal(
    basic_hdbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      min_points = rlang::expr(missing_arg())
    )
  )

  db_hdbscan <- translate_tidyclust(
    db_clust(min_points = 4) |> set_engine("hdbscan")
  )
  expect_equal(
    db_hdbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      min_points = rlang::expr(missing_arg()),
      min_points = new_empty_quosure(4)
    )
  )
})


test_that("bad input", {
  expect_snapshot(error = TRUE, db_clust(mode = "bogus"))

  skip_if_not_installed("dbscan")

  expect_snapshot(error = TRUE, {
    bt <- db_clust(radius = -1) %>% set_engine("dbscan")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, {
    bt <- db_clust(min_points = -1) %>% set_engine("dbscan")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, translate_tidyclust(db_clust(), engine = NULL))
  expect_snapshot(error = TRUE, translate_tidyclust(db_clust(formula = ~x)))
})

test_that("predictions", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  db_clust_fit <- db_clust(radius = 60, min_points = 5) %>%
    set_engine("dbscan") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- dbscan::dbscan(mtcars, eps = 60, minPts = 5)$cluster

  ref_clusts <- ref_res[ref_res != 0]
  ref_outliers <- which(ref_res == 0)

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    ref_clusts,
    predict(db_clust_fit, mtcars)$.pred_cluster %>%
      .[. != "Outlier"] %>%
      as.numeric() -
      1
  )

  expect_equal(
    relevel_preds(ref_clusts),
    extract_cluster_assignment(db_clust_fit)$.cluster %>%
      .[. != "Outlier"] %>%
      as.numeric() -
      1
  )

  expect_equal(
    ref_outliers,
    which(predict(db_clust_fit, mtcars)$.pred_cluster == "Outlier")
  )
})


test_that("extract_centroids work", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  dbscan_fit <- db_clust(radius = 20, min_points = 3) %>%
    set_engine("dbscan") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- dbscan::dbscan(mtcars, eps = 20, minPts = 3)

  centers <- bind_cols(mtcars, .cluster = ref_res$cluster) %>%
    dplyr::group_by(.cluster) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::filter(.cluster != 0) %>%
    dplyr::select(-.cluster)

  ref_centroids <- dplyr::bind_cols(
    .cluster = c(1L, 2L, 4L, 5L, 3L),
    centers
  ) %>%
    dplyr::arrange(.cluster) %>%
    dplyr::select(-.cluster)

  expect_identical(
    extract_centroids(dbscan_fit) %>%
      dplyr::filter(.cluster != "Outlier") %>%
      dplyr::select(-.cluster),
    ref_centroids
  )
})

test_that("Right classes", {
  expect_equal(
    class(db_clust()),
    c("db_clust", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    db_clust()
  )
  expect_snapshot(
    db_clust(radius = 20, min_points = 3)
  )
})

test_that("updating", {
  expect_snapshot(
    db_clust(radius = 20, min_points = 5) %>%
      update(radius = tune())
  )
})


test_that("reordering is done correctly for dbscan db_clust", {
  skip_if_not_installed("dbscan")
  set.seed(42)

  dbscan_fit <- db_clust(radius = 20, min_points = 3) %>%
    set_engine("dbscan") %>%
    fit(~., data = mtcars)

  summ <- extract_fit_summary(dbscan_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})


test_that("errors if `radius` and `min_points` aren't specified", {
  skip_if_not_installed("dbscan")
  expect_snapshot(
    error = TRUE,
    db_clust() %>%
      set_engine("dbscan") %>%
      fit(~., data = mtcars)
  )
})

test_that("errors if `radius` isn't specified", {
  skip_if_not_installed("dbscan")
  expect_snapshot(
    error = TRUE,
    db_clust(min_points = 10) %>%
      set_engine("dbscan") %>%
      fit(~., data = mtcars)
  )
})

test_that("errors if `min_points` isn't specified", {
  skip_if_not_installed("dbscan")
  expect_snapshot(
    error = TRUE,
    db_clust(radius = 20) %>%
      set_engine("dbscan") %>%
      fit(~., data = mtcars)
  )
})

# ------------------------------------------------------------------------------
# hdbscan engine

test_that("predictions (hdbscan)", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  x_scaled <- as.data.frame(scale(mtcars))
  hdb_fit <- db_clust(min_points = 5) |>
    set_engine("hdbscan") |>
    fit(~., x_scaled)

  preds <- predict(hdb_fit, x_scaled)
  expect_s3_class(preds$.pred_cluster, "factor")
  expect_identical(nrow(preds), nrow(mtcars))

  assignments <- extract_cluster_assignment(hdb_fit)
  expect_identical(
    levels(preds$.pred_cluster),
    levels(assignments$.cluster)
  )
})

test_that("predictions on new data (hdbscan)", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  x_scaled <- as.data.frame(scale(mtcars))
  train <- x_scaled[1:24, ]
  new <- x_scaled[25:32, ]

  hdb_fit <- db_clust(min_points = 5) |>
    set_engine("hdbscan") |>
    fit(~., train)

  preds <- predict(hdb_fit, new)
  expect_s3_class(preds$.pred_cluster, "factor")
  expect_identical(nrow(preds), nrow(new))
  expect_identical(
    levels(preds$.pred_cluster),
    levels(extract_cluster_assignment(hdb_fit)$.cluster)
  )
})

test_that("extract_cluster_assignment works (hdbscan)", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  x_scaled <- as.data.frame(scale(mtcars))
  hdb_fit <- db_clust(min_points = 5) |>
    set_engine("hdbscan") |>
    fit(~., x_scaled)

  assigned <- extract_cluster_assignment(hdb_fit)$.cluster
  expect_identical(
    which(assigned == "Outlier"),
    which(hdb_fit$fit$cluster == 0)
  )
  expect_identical(
    length(unique(assigned[assigned != "Outlier"])),
    length(unique(hdb_fit$fit$cluster[hdb_fit$fit$cluster != 0]))
  )
})

test_that("extract_centroids works (hdbscan)", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  x_scaled <- as.data.frame(scale(mtcars))
  hdb_fit <- db_clust(min_points = 5) |>
    set_engine("hdbscan") |>
    fit(~., x_scaled)

  centroids <- extract_centroids(hdb_fit)
  expect_named(centroids, c(".cluster", colnames(mtcars)))
  expect_identical(
    levels(centroids$.cluster),
    levels(extract_cluster_assignment(hdb_fit)$.cluster)
  )

  # the "Outlier" row should be NA
  outlier_row <- centroids[centroids$.cluster == "Outlier", -1]
  expect_all_true(is.na(unlist(outlier_row)))
})

test_that("min_cluster_size overrides min_points (hdbscan)", {
  skip_if_not_installed("dbscan")
  set.seed(1234)
  x <- as.data.frame(scale(mtcars))

  fit_default <- db_clust(min_points = 5) |>
    set_engine("hdbscan") |>
    fit(~., x)

  fit_override <- db_clust(min_points = 5) |>
    set_engine("hdbscan", min_cluster_size = 10) |>
    fit(~., x)

  expect_identical(fit_default$fit$minPts, 5)
  expect_identical(fit_override$fit$minPts, 10)
})

test_that("errors if `min_points` isn't specified (hdbscan)", {
  skip_if_not_installed("dbscan")
  expect_snapshot(
    error = TRUE,
    db_clust() |>
      set_engine("hdbscan") |>
      fit(~., data = mtcars)
  )
})
