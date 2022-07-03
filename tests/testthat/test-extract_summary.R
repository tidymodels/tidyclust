test_that("extract summary works for kmeans", {
  obj1 <- k_means(k = mtcars[1:3, ]) %>%
    set_engine_tidyclust("stats", algorithm = "MacQueen") %>%
    fit(~., mtcars)

  obj2 <- k_means(k = 3) %>%
    set_engine_tidyclust("ClusterR", CENTROIDS = as.matrix(mtcars[1:3, ])) %>%
    fit(~., mtcars)

  summ1 <- extract_fit_summary(obj1)
  summ2 <- extract_fit_summary(obj2)

  expect_equal(names(summ1), names(summ2))

  # check order
  expect_equal(summ1$n_members, c(17, 11, 4))
})
