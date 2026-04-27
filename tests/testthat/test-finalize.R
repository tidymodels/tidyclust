test_that("finalize_model_tidyclust() updates parameters", {
  withr::local_options(lifecycle_verbosity = "quiet")

  spec <- k_means(num_clusters = tune())
  params <- data.frame(num_clusters = 5)

  res <- finalize_model_tidyclust(spec, params)

  expect_s3_class(res, "cluster_spec")
  expect_equal(res$args$num_clusters, new_empty_quosure(5))
})

test_that("finalize_model_tidyclust() works with tibble parameters", {
  withr::local_options(lifecycle_verbosity = "quiet")

  spec <- k_means(num_clusters = tune())
  params <- tibble::tibble(num_clusters = 3)

  res <- finalize_model_tidyclust(spec, params)

  expect_s3_class(res, "cluster_spec")
  expect_equal(res$args$num_clusters, new_empty_quosure(3))
})

test_that("finalize_model_tidyclust() errors on non-cluster_spec", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(
    error = TRUE,
    finalize_model_tidyclust("not a spec", data.frame(num_clusters = 5))
  )
})

test_that("tune::finalize_model() works with cluster_spec", {
  spec <- k_means(num_clusters = tune())
  params <- data.frame(num_clusters = 5)

  res <- tune::finalize_model(spec, params)

  expect_s3_class(res, "cluster_spec")
  expect_equal(res$args$num_clusters, new_empty_quosure(5))
})

test_that("tune::finalize_workflow() works with cluster_spec workflows", {
  spec <- k_means(num_clusters = tune())
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(~.)
  params <- data.frame(num_clusters = 4)

  res <- tune::finalize_workflow(wf, params)

  expect_s3_class(res, "workflow")
  extracted_spec <- workflows::extract_spec_parsnip(res)
  expect_equal(extracted_spec$args$num_clusters, new_empty_quosure(4))
})

test_that("finalize_model_tidyclust() is deprecated", {
  spec <- k_means(num_clusters = tune())
  params <- data.frame(num_clusters = 5)

  expect_snapshot(. <- finalize_model_tidyclust(spec, params))
})

test_that("finalize_workflow_tidyclust() works with workflows", {
  withr::local_options(lifecycle_verbosity = "quiet")

  spec <- k_means(num_clusters = tune())
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(~.)

  params <- data.frame(num_clusters = 4)

  res <- finalize_workflow_tidyclust(wf, params)

  expect_s3_class(res, "workflow")
  extracted_spec <- workflows::extract_spec_parsnip(res)
  expect_equal(extracted_spec$args$num_clusters, new_empty_quosure(4))
})

test_that("finalize_workflow_tidyclust() errors on non-workflow", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(
    error = TRUE,
    finalize_workflow_tidyclust("not a workflow", data.frame(num_clusters = 5))
  )
})

test_that("finalize_workflow_tidyclust() is deprecated", {
  spec <- k_means(num_clusters = tune())
  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(~.)
  params <- data.frame(num_clusters = 4)

  expect_snapshot(. <- finalize_workflow_tidyclust(wf, params))
})
