# control class

    Code
      fit(x, mpg ~ ., data = mtcars, control = ctrl)
    Error <rlang_error>
      The 'control' argument should have class 'control_celery'.

---

    Code
      fit_xy(x, x = mtcars[, -1], y = mtcars$mpg, control = ctrl)
    Error <rlang_error>
      The 'control' argument should have class 'control_celery'.

