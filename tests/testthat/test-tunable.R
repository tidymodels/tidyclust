test_that("tunable.k_means returns expected parameters", {
  spec <- k_means(num_clusters = tune()) |>
    set_engine("stats")

  res <- tunable(spec)

  expected <- tibble::tibble(
    name = "num_clusters",
    call_info = list(list(pkg = "dials", fun = "num_clusters")),
    source = "cluster_spec",
    component = "k_means",
    component_id = "main"
  )

  expect_identical(res, expected)
})

test_that("tunable.hier_clust returns expected parameters", {
  spec <- hier_clust(num_clusters = tune()) |>
    set_engine("stats")

  res <- tunable(spec)

  expected <- tibble::tibble(
    name = c("num_clusters", "linkage_method", "cut_height"),
    call_info = list(
      list(pkg = "dials", fun = "num_clusters"),
      list(pkg = "tidyclust", fun = "linkage_method"),
      list(pkg = "tidyclust", fun = "cut_height")
    ),
    source = "cluster_spec",
    component = "hier_clust",
    component_id = "main"
  )

  expect_identical(res, expected)
})

test_that("tunable returns tibble for models with fixed params", {
  spec <- k_means(num_clusters = 3) |>
    set_engine("stats")

  res <- tunable(spec)

  expected <- tibble::tibble(
    name = "num_clusters",
    call_info = list(list(pkg = "dials", fun = "num_clusters")),
    source = "cluster_spec",
    component = "k_means",
    component_id = "main"
  )

  expect_identical(res, expected)
})
