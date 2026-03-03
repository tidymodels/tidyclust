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

# k_means errors when num_clusters > distinct data points

    Code
      fit(set_engine(k_means(num_clusters = 10), "stats"), ~., data = small_data)
    Condition
      Error in `fit()`:
      ! `num_clusters` must be at most the number of distinct data points (4).
      i `num_clusters` was set to 10.

