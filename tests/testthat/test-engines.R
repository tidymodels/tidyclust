test_that("set_engine() errors with no engine set", {
  expect_snapshot(
    error = TRUE,
    set_engine(k_means())
  )
})

# k_means engines

test_that("k_means fits with stats engine", {
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  expect_no_condition(
    fit <- fit(spec, ~., data = mtcars)
  )

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "stats")

  preds <- predict(fit, new_data = mtcars)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_cluster")
  expect_equal(nrow(preds), nrow(mtcars))
})

test_that("k_means fits with ClusterR engine", {
  skip_if_not_installed("ClusterR")

  spec <- k_means(num_clusters = 3) |>
    set_engine("ClusterR")

  expect_no_condition(
    fit <- fit(spec, ~., data = mtcars)
  )

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "ClusterR")

  preds <- predict(fit, new_data = mtcars)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_cluster")
  expect_equal(nrow(preds), nrow(mtcars))
})

test_that("k_means fits with klaR engine", {
  skip_if_not_installed("klaR")

  cat_data <- data.frame(
    x = factor(sample(letters[1:3], 50, replace = TRUE)),
    y = factor(sample(letters[4:6], 50, replace = TRUE)),
    z = factor(sample(letters[7:9], 50, replace = TRUE))
  )

  spec <- k_means(num_clusters = 2) |>
    set_engine("klaR")

  expect_no_condition(
    fit <- fit(spec, ~., data = cat_data)
  )

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "klaR")

  preds <- predict(fit, new_data = cat_data)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_cluster")
  expect_equal(nrow(preds), nrow(cat_data))
})

test_that("k_means fits with clustMixType engine", {
  skip_if_not_installed("clustMixType")

  mixed_data <- data.frame(
    num1 = rnorm(50),
    num2 = rnorm(50),
    cat1 = factor(sample(letters[1:3], 50, replace = TRUE))
  )

  spec <- k_means(num_clusters = 2) |>
    set_engine("clustMixType")

  expect_no_condition(
    fit <- fit(spec, ~., data = mixed_data)
  )

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "clustMixType")

  preds <- predict(fit, new_data = mixed_data)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_cluster")
  expect_equal(nrow(preds), nrow(mixed_data))
})

# hier_clust engines

test_that("hier_clust fits with stats engine", {
  spec <- hier_clust(num_clusters = 3) |>
    set_engine("stats")

  expect_no_condition(
    fit <- fit(spec, ~., data = mtcars)
  )

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$spec$engine, "stats")

  preds <- predict(fit, new_data = mtcars)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_cluster")
  expect_equal(nrow(preds), nrow(mtcars))
})

# Engine consistency tests

test_that("k_means stats and ClusterR produce consistent cluster counts", {
  skip_if_not_installed("ClusterR")

  set.seed(123)
  n_clusters <- 3

  fit_stats <- k_means(num_clusters = n_clusters) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  fit_clusterr <- k_means(num_clusters = n_clusters) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  preds_stats <- predict(fit_stats, new_data = mtcars)
  preds_clusterr <- predict(fit_clusterr, new_data = mtcars)

  expect_equal(length(unique(preds_stats$.pred_cluster)), n_clusters)
  expect_equal(length(unique(preds_clusterr$.pred_cluster)), n_clusters)
})

test_that("extract_centroids works across engines", {
  skip_if_not_installed("ClusterR")

  n_clusters <- 3

  fit_stats <- k_means(num_clusters = n_clusters) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  fit_clusterr <- k_means(num_clusters = n_clusters) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  centroids_stats <- extract_centroids(fit_stats)
  centroids_clusterr <- extract_centroids(fit_clusterr)

  expect_s3_class(centroids_stats, "tbl_df")
  expect_s3_class(centroids_clusterr, "tbl_df")
  expect_equal(nrow(centroids_stats), n_clusters)
  expect_equal(nrow(centroids_clusterr), n_clusters)
  expect_true(".cluster" %in% names(centroids_stats))
  expect_true(".cluster" %in% names(centroids_clusterr))
})

test_that("extract_cluster_assignment works across engines", {
  skip_if_not_installed("ClusterR")

  n_clusters <- 3

  fit_stats <- k_means(num_clusters = n_clusters) |>
    set_engine("stats") |>
    fit(~., data = mtcars)

  fit_clusterr <- k_means(num_clusters = n_clusters) |>
    set_engine("ClusterR") |>
    fit(~., data = mtcars)

  assignments_stats <- extract_cluster_assignment(fit_stats)
  assignments_clusterr <- extract_cluster_assignment(fit_clusterr)

  expect_s3_class(assignments_stats, "tbl_df")
  expect_s3_class(assignments_clusterr, "tbl_df")
  expect_equal(nrow(assignments_stats), nrow(mtcars))
  expect_equal(nrow(assignments_clusterr), nrow(mtcars))
  expect_named(assignments_stats, ".cluster")
  expect_named(assignments_clusterr, ".cluster")
})
