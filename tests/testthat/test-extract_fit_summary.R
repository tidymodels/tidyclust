test_that("extract summary works for kmeans", {
  obj1 <- k_means(num_clusters = mtcars[1:3, ]) |>
    set_engine("stats", algorithm = "MacQueen") |>
    fit(~., mtcars)

  obj2 <- k_means(num_clusters = 3) |>
    set_engine("ClusterR", CENTROIDS = as.matrix(mtcars[1:3, ])) |>
    fit(~., mtcars)

  summ1 <- extract_fit_summary(obj1)
  summ2 <- extract_fit_summary(obj2)

  expect_equal(names(summ1), names(summ2))

  # check order
  expect_equal(summ1$n_members, c(17, 11, 4))

  expect_true(is.factor(summ1$cluster_names))
  expect_true(is.factor(summ1$cluster_assignments))

  expect_true(is.factor(summ2$cluster_names))
  expect_true(is.factor(summ2$cluster_assignments))
})

test_that("extract summary works for kmeans when num_clusters = 1", {
  obj1 <- k_means(num_clusters = 1) |>
    set_engine("stats") |>
    fit(~., mtcars)

  obj2 <- k_means(num_clusters = 1) |>
    set_engine("ClusterR") |>
    fit(~., mtcars)

  summ1 <- extract_fit_summary(obj1)
  summ2 <- extract_fit_summary(obj2)

  expect_equal(
    summ1$centroids,
    tibble::as_tibble(lapply(mtcars, mean))
  )

  expect_equal(
    summ2$centroids,
    tibble::as_tibble(lapply(mtcars, mean))
  )
})

test_that("extract summary works for hier_clust", {
  obj1 <- tidyclust::hier_clust(num_clusters = 4) |>
    fit(~., mtcars)

  summ1 <- extract_fit_summary(obj1)

  expect_true(is.factor(summ1$cluster_names))
  expect_true(is.factor(summ1$cluster_assignments))
})

test_that("extract_fit_summary() errors for cluster spec", {
  spec <- tidyclust::k_means(num_clusters = 4)

  expect_snapshot(
    error = TRUE,
    extract_fit_summary(spec)
  )
})

test_that("prefix is passed in extract_fit_summary()", {
  spec <- tidyclust::k_means(num_clusters = 4) |>
    fit(~., data = mtcars)

  res <- extract_fit_summary(spec, prefix = "C_")

  expect_true(
    all(substr(res$.cluster, 1, 2) == "C_")
  )
})
