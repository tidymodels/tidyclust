toy_df <- data.frame(
  "beer"    = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  "milk"    = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  "bread"   = c(TRUE, TRUE, FALSE, TRUE, TRUE),
  "diapers" = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  "eggs"    = c(FALSE, TRUE, FALSE, FALSE, FALSE)
)

toy_pred <- data.frame(
  "beer"    = FALSE,
  "milk"    = NA,
  "bread"   = TRUE,
  "diapers" = TRUE,
  "eggs"    = FALSE
)

test_that("primary arguments", {
  skip_if_not_installed("arules")
  basic <- freq_itemsets(mode = "partition")
  basic_arules <- translate_tidyclust(basic %>% set_engine("arules"))
  expect_equal(
    basic_arules$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      mining_method = new_empty_quosure("eclat")
    )
  )

  fi <- freq_itemsets(min_support = 0.5, mining_method = "apriori", mode = "partition")
  fi_arules <- translate_tidyclust(fi %>% set_engine("arules"))
  expect_equal(
    fi_arules$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      min_support = new_empty_quosure(0.5),
      mining_method = new_empty_quosure("apriori")
    )
  )
})

test_that("bad input", {
  skip_if_not_installed("arules")
  expect_snapshot(error = TRUE, freq_itemsets(mode = "bogus"))
  expect_snapshot(error = TRUE, {
    bt <- freq_itemsets(min_support = 0.05, mining_method = "bogus")
    fit(bt, ~ ., toy_df)
    })
  expect_snapshot(error = TRUE, {
    bt <- freq_itemsets(min_support = -1, mining_method = "eclat") %>% set_engine("arules")
    fit(bt, ~ ., toy_df)
    })
  expect_snapshot(error = TRUE, translate_tidyclust(freq_itemsets(), engine = NULL))
  expect_snapshot(error = TRUE, translate_tidyclust(freq_itemsets(formula = ~x)))
})

test_that("clusters", {
  set.seed(1234)
  skip_if_not_installed("arules")
  fi_fit <- freq_itemsets(min_support = 0.5, mining_method = "apriori") %>%
    set_engine("arules") %>%
    fit(~., toy_df %>% dplyr::mutate(across(everything(), as.numeric)))

  set.seed(1234)
  ref_res <- arules::apriori(data = toy_df,
                             parameter = list(support = 0.5, target = "frequent itemsets"),
                             control = list(verbose = FALSE))

  ref_itemsets <- arules::DATAFRAME(ref_res)
  ref_clusts <- c(1, 2, 2, 2, 0)
  ref_outliers <- "eggs"

  expect_equal(
    arules::DATAFRAME(fi_fit$fit),
    ref_itemsets
  )

  expect_equal(
    ref_clusts,
    extract_cluster_assignment(fi_fit)$.cluster %>% as.numeric() - 1
  )
})

test_that("predict", {
  set.seed(1234)
  skip_if_not_installed("arules")
  fi_fit <- freq_itemsets(min_support = 0.5, mining_method = "apriori") %>%
    set_engine("arules") %>%
    fit(~., toy_df)

  ref_pred_raw <- c(NA, 0.766666666667, NA, NA, NA)
  ref_pred_thresh <- c(NA, 1, NA, NA, NA)

  expect_equal(
    ref_pred_thresh,
    predict(fi_fit, toy_pred)$.pred_cluster[[1]]$.pred_item
  )

  expect_equal(
    ref_pred_raw,
    predict(fi_fit, toy_pred, type = "raw")$.pred_cluster[[1]]$.pred_item
  )
})

test_that("extract_centroids work", {
  set.seed(1234)
  skip_if_not_installed("arules")
  fi_fit <- freq_itemsets(min_support = 0.5) %>%
    set_engine("arules") %>%
    fit(~., toy_df %>% dplyr::mutate(across(everything(), as.numeric)))

  expect_snapshot(error = TRUE, extract_centroids(fi_fit))
})

test_that("Right classes", {
  skip_if_not_installed("arules")
  expect_equal(
    class(freq_itemsets()),
    c("freq_itemsets", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  skip_if_not_installed("arules")
  expect_snapshot(
    freq_itemsets()
  )
  expect_snapshot(
    freq_itemsets(min_support = 0.5)
  )
})

test_that("updating", {
  skip_if_not_installed("arules")
  expect_snapshot(
    freq_itemsets(min_support = 0.5) %>%
      update(min_support = tune())
  )
})

test_that("errors if `min_support` isn't specified", {
  skip_if_not_installed("arules")
  expect_snapshot(
    error = TRUE,
    freq_itemsets() %>%
      set_engine("arules") %>%
      fit(~ ., data = toy_df)
  )
})
