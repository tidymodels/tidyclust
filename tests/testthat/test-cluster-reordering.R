test_that("first seen cluster is assigned as Cluster_1", {
  set.seed(123)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  assignments <- extract_cluster_assignment(fit)

  expect_identical(as.character(assignments$.cluster[1]), "Cluster_1")
})

test_that("reordering is consistent across k_means engines", {
  skip_if_not_installed("ClusterR")

  set.seed(123)

  fit_stats <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  set.seed(123)
  fit_clusterr <- k_means(num_clusters = 3) |>
    set_engine("ClusterR") |>
    fit(~., mtcars)

  assignments_stats <- extract_cluster_assignment(fit_stats)
  assignments_clusterr <- extract_cluster_assignment(fit_clusterr)

  expect_identical(
    as.character(assignments_stats$.cluster[1]),
    "Cluster_1"
  )
  expect_identical(
    as.character(assignments_clusterr$.cluster[1]),
    "Cluster_1"
  )
})

test_that("centroids are actual means of cluster members", {
  set.seed(123)
  fit <- k_means(num_clusters = 3) |>
    set_engine("stats") |>
    fit(~., mtcars)

  assignments <- extract_cluster_assignment(fit)
  centroids <- extract_centroids(fit)

  expected_centroids <- mtcars |>
    dplyr::mutate(.cluster = assignments$.cluster) |>
    dplyr::group_by(.cluster) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), mean))

  expect_equal(
    centroids |> dplyr::arrange(.cluster),
    expected_centroids |> dplyr::arrange(.cluster),
    tolerance = 1e-10
  )
})
