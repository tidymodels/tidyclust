# fit and fit_xy errors if outcome is provided

    Code
      k_means(num_clusters = 5) %>% fit_xy(mtcars, y = mtcars$mpg)
    Error <rlang_error>
      Outcomes are not used in `cluster_spec` objects.

---

    Code
      workflows::workflow(mpg ~ ., km) %>% fit(mtcars)
    Error <rlang_error>
      Outcomes are not used in `cluster_spec` objects.

