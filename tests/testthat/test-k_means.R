library(testthat)
library(parsnip)
library(rlang)

source(test_path("helper-functions.R"))

# ------------------------------------------------------------------------------

test_that("primary arguments", {
  basic <- k_means(mode = "partition")
  basic_stats <- translate_celery(basic %>% set_engine_celery("stats"))
  expect_equal(
    basic_stats$method$fit$args,
    list(
      x = expr(missing_arg()),
      centers = expr(missing_arg())
    )
  )

  k <- k_means(k = 15, mode = "partition")
  k_stats <- translate_celery(k %>% set_engine_celery("stats"))
  expect_equal(
    k_stats$method$fit$args,
    list(
      x = expr(missing_arg()),
      centers = expr(missing_arg()),
      centers = new_empty_quosure(15)
    )
  )
})

test_that("engine arguments", {
  stats_print <- k_means(mode = "partition")
  expect_equal(
    translate_celery(
      stats_print %>%
        set_engine_celery("stats", nstart = 1L)
    )$method$fit$args,
    list(
      x = expr(missing_arg()),
      centers = expr(missing_arg()),
      nstart = new_empty_quosure(1L)
    )
  )
})

test_that("bad input", {
  expect_snapshot(error = TRUE, k_means(mode = "bogus"))
  expect_snapshot(error = TRUE, {
    bt <- k_means(k = -1) %>% set_engine_celery("stats")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, translate_celery(k_means(), engine = NULL))
  expect_snapshot(error = TRUE, translate_celery(k_means(formula = ~x)))
})

# ------------------------------------------------------------------------------
set.seed(1234)
kmeans_fit <- k_means(k = 4) %>%
  set_engine_celery("stats") %>%
  fit(~., mtcars)

set.seed(1234)
ref_res <- kmeans(mtcars, 4)

ref_predictions <- ref_res$centers %>%
  flexclust::dist2(mtcars) %>%
  apply(2, which.min) %>%
  unname()

# replaced with test in test-predict_formats.R
# should check that clusterings are "essentially equivalent"
# expect_equal(
#   ref_predictions,
#   predict(kmeans_fit, mtcars)$.pred_cluster %>% as.numeric()
# )
#
# expect_equal(
#   unname(ref_res$cluster),
#   extract_cluster_assignment(kmeans_fit)$.cluster %>% as.numeric()
# )
#
# expect_equal(
#   predict(kmeans_fit, mtcars)$.pred_cluster %>% as.numeric(),
#   extract_cluster_assignment(kmeans_fit)$.cluster %>% as.numeric()
# )

# ------------------------------------------------------------------------------

test_that("Right classes", {
  expect_equal(class(k_means()), c("k_means", "cluster_spec"))
})

test_that("printing", {
  expect_snapshot(
    k_means()
  )
  expect_snapshot(
    k_means(k = 10)
  )
})
