# fit() and fit_xy() errors if outcome is provided

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

# k_means() errors when num_clusters > distinct data points

    Code
      fit(set_engine(k_means(num_clusters = 10), "stats"), ~., data = small_data)
    Condition
      Error in `fit()`:
      ! `num_clusters` must be at most the number of distinct data points (4).
      i `num_clusters` was set to 10.

# fit() errors when mode is unknown

    Code
      fit(spec, ~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please set the mode in the model specification.

# fit() uses default engine when not set and verbosity > 0

    Code
      fit <- fit(k_means(num_clusters = 3, engine = NULL), ~., data = mtcars[1:10, ],
      control = control_cluster(verbosity = 1))
    Condition
      Warning:
      Engine set to `stats`.

# fit_xy() uses default engine when not set and verbosity > 0

    Code
      fit <- fit_xy(k_means(num_clusters = 3, engine = NULL), mtcars[1:10, ],
      control = control_cluster(verbosity = 1))
    Condition
      Warning:
      Engine set to `stats`.

# fit() errors when called with x and y arguments

    Code
      fit(set_engine(k_means(num_clusters = 3), "stats"), ~., data = mtcars, x = mtcars,
      y = mtcars$mpg)
    Condition
      Error in `fit()`:
      ! The `fit.cluster_spec()` function is for the formula methods.  Use `fit_xy()` instead.

# fit() errors when formula is not a formula

    Code
      fit(set_engine(k_means(num_clusters = 3), "stats"), "not a formula", data = mtcars)
    Condition
      Error in `inher()`:
      ! `formula` should be a string.

# fit_xy() errors when x has no column names

    Code
      fit_xy(set_engine(k_means(num_clusters = 3), "stats"), mat)
    Condition
      Error in `fit_xy()`:
      ! `x` should have column names.

