test_that("primary arguments", {
  basic <- hier_clust(mode = "partition")
  basic_stats <- translate_tidyclust(basic %>% set_engine_tidyclust("stats"))
  expect_equal(
    basic_stats$method$fit$args,
    list(
      x = rlang::expr(missing_arg())
    )
  )
})

test_that("engine arguments", {
  stats_print <- hier_clust(mode = "partition")
  expect_equal(
    translate_tidyclust(
      stats_print %>%
        set_engine_tidyclust("stats", linkage_method = "single")
    )$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      nstart = new_empty_quosure("single")
    )
  )
})

test_that("bad input", {
  expect_snapshot(error = TRUE, hier_clust(mode = "bogus"))
  expect_snapshot(error = TRUE, {
    bt <- hier_clust(linkage_method = "bogus") %>% set_engine_tidyclust("stats")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, translate_tidyclust(hier_clust(), engine = NULL))
  expect_snapshot(error = TRUE, translate_tidyclust(hier_clust(formula = ~x)))
})

test_that("predictions", {
  set.seed(1234)
  hclust_fit <- hier_clust(k = 4) %>%
    set_engine_tidyclust("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res %>% unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    relevel_preds(ref_predictions),
    predict(hclust_fit, mtcars)$.pred_cluster %>% as.numeric()
  )

  expect_equal(
    relevel_preds(unname(ref_res$cluster)),
    extract_cluster_assignment(hclust_fit)$.cluster %>% as.numeric()
  )

  expect_equal(
    relevel_preds(predict(hclust_fit, mtcars)$.pred_cluster),
    extract_cluster_assignment(hclust_fit)$.cluster %>% as.numeric()
  )
})

test_that("Right classes", {
  expect_equal(class(hier_clust()), c("hier_clust", "cluster_spec"))
})

test_that("printing", {
  expect_snapshot(
    hier_clust()
  )
  expect_snapshot(
    hier_clust(k = 10)
  )
})

test_that('updating', {
  expect_snapshot(
    hier_clust(k = 5) %>%
      update(k = tune())
  )
})
