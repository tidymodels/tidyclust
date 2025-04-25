# fit and fit_xy errors if outcome is provided

    Code
      fit_xy(k_means(num_clusters = 5), mtcars, y = mtcars$mpg)
    Condition
      Error in `x_x()`:
      ! Outcomes are not used in <cluster_spec> objects.

---

    Code
      fit(workflows::workflow(mpg ~ ., km), mtcars)
    Condition
      Error in `x_x()`:
      ! Outcomes are not used in <cluster_spec> objects.

