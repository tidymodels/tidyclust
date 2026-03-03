test_that("finalize_model_tidyclust updates parameters", {
  spec <- k_means(num_clusters = tune())
  params <- data.frame(num_clusters = 5)

  res <- finalize_model_tidyclust(spec, params)

  expect_s3_class(res, "cluster_spec")
  expect_equal(res$args$num_clusters, new_empty_quosure(5))
})

test_that("finalize_model_tidyclust works with tibble parameters", {
  spec <- k_means(num_clusters = tune())
  params <- tibble::tibble(num_clusters = 3)

  res <- finalize_model_tidyclust(spec, params)

  expect_s3_class(res, "cluster_spec")
  expect_equal(res$args$num_clusters, new_empty_quosure(3))
})

test_that("finalize_model_tidyclust errors on non-cluster_spec", {
  expect_snapshot(
    error = TRUE,
    finalize_model_tidyclust("not a spec", data.frame(num_clusters = 5))
  )
})

test_that("finalize_workflow_tidyclust works with workflows", {
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

test_that("finalize_workflow_tidyclust errors on non-workflow", {
  expect_snapshot(
    error = TRUE,
    finalize_workflow_tidyclust("not a workflow", data.frame(num_clusters = 5))
  )
})
