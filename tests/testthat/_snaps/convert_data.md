# .convert_form_to_x_fit() errors on invalid composition

    Code
      .convert_form_to_x_fit(~., mtcars, composition = "invalid")
    Condition
      Error in `.convert_form_to_x_fit()`:
      ! `composition` should be <data.frame> or <matrix>.

# .convert_form_to_x_fit() errors on non-numeric weights

    Code
      .convert_form_to_x_fit(~., mtcars, weights = letters[1:32])
    Condition
      Error in `.convert_form_to_x_fit()`:
      ! The `weights` must be a numeric vector.

# .convert_form_to_x_fit() errors on invalid dots arguments

    Code
      .convert_form_to_x_fit(~., mtcars, bad_arg = 1)
    Condition
      Error in `check_form_dots()`:
      ! The argument `bad_arg` cannot be used to create the data.
      i Possible arguments are: `subset` and `weights`.

# .convert_form_to_x_new() errors on invalid composition

    Code
      .convert_form_to_x_new(fit$preproc, mtcars, composition = "invalid")
    Condition
      Error in `.convert_form_to_x_new()`:
      ! `composition` should be either `data.frame` or `matrix`.

