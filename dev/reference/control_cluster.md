# Control the fit function

Options can be passed to the
[`fit.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
function that control the output and computations.

## Usage

``` r
control_cluster(verbosity = 1L, catch = FALSE)

# S3 method for class 'control_cluster'
print(x, ...)
```

## Arguments

- verbosity:

  An integer where a value of zero indicates that no messages or output
  should be shown when packages are loaded or when the model is fit. A
  value of 1 means that package loading is quiet but model fits can
  produce output to the screen (depending on if they contain their own
  `verbose`-type argument). A value of 2 or more indicates that any
  output should be seen.

- catch:

  A logical where a value of `TRUE` will evaluate the model inside of
  `try(, silent = TRUE)`. If the model fails, an object is still
  returned (without an error) that inherits the class "try-error".

- x:

  A `control_cluster` object.

- ...:

  Not currently used.

## Value

An S3 object with class "control_cluster" that is a named list with the
results of the function call

The input `x`, invisibly.

## Examples

``` r
control_cluster()
#> tidyclust control object

# Catch errors instead of stopping — useful inside loops or tune_cluster()
control_cluster(catch = TRUE)
#> tidyclust control object
#>  - fit errors will be caught

# Suppress all output during fitting
control_cluster(verbosity = 0L)
#> tidyclust control object

# Show model output but suppress package loading messages (default)
control_cluster(verbosity = 1L)
#> tidyclust control object

# Show all output including package loading messages
control_cluster(verbosity = 2L)
#> tidyclust control object
#>  - verbose level 2 
```
