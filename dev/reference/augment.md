# Augment data with predictions

[`augment()`](https://generics.r-lib.org/reference/augment.html) will
add column(s) for predictions to the given data.

## Usage

``` r
# S3 method for class 'cluster_fit'
augment(x, new_data, ...)
```

## Arguments

- x:

  A
  [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
  object produced by
  [`fit.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
  or
  [`fit_xy.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
  .

- new_data:

  A data frame or matrix.

- ...:

  Not currently used.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with containing `new_data` with columns added depending on the mode of
the model.

## Details

For partition models, a `.pred_cluster` column is added.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  augment(new_data = mtcars)
#> # A tibble: 32 × 12
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: .pred_cluster <fct>
```
