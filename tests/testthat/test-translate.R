test_that("k_means translation for stats engine", {
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})

test_that("k_means translation for ClusterR engine", {
  skip_if_not_installed("ClusterR")

  spec <- k_means(num_clusters = 3) |>
    set_engine("ClusterR")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})

test_that("k_means translation for klaR engine", {
  skip_if_not_installed("klaR")

  spec <- k_means(num_clusters = 3) |>
    set_engine("klaR")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})

test_that("k_means translation for clustMixType engine", {
  skip_if_not_installed("clustMixType")

  spec <- k_means(num_clusters = 3) |>
    set_engine("clustMixType")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})

test_that("hier_clust translation for stats engine", {
  spec <- hier_clust(num_clusters = 3) |>
    set_engine("stats")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})

test_that("hier_clust translation with linkage_method", {
  spec <- hier_clust(num_clusters = 3, linkage_method = "ward.D") |>
    set_engine("stats")

  expect_snapshot(
    translate_tidyclust(spec)
  )
})
