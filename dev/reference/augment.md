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
  [`fit_xy.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md).

- new_data:

  A data frame or matrix.

- ...:

  Not currently used.

## Value

A tibble containing `new_data` with a `.pred_cluster` column appended
giving the cluster assignment for each row.

## Details

For partition models, a `.pred_cluster` column is added.

### Preprocessing with workflows

When `x` is a fitted
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
that includes a recipe, the recipe transformations are applied to
`new_data` before predicting. The returned tibble contains the
**original** (untransformed) `new_data` plus the `.pred_cluster` column,
so the data is not altered by preprocessing.

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

# With a workflow that includes a recipe
library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
library(workflows)

rec <- recipe(~., data = mtcars) |>
  step_normalize(all_predictors())

wf_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(kmeans_spec) |>
  fit(data = mtcars)

# Returns original (untransformed) data with .pred_cluster appended
augment(wf_fit, new_data = mtcars)
#> # A tibble: 32 × 12
#>    .pred_cluster   mpg   cyl  disp    hp  drat    wt  qsec    vs    am
#>  * <fct>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Cluster_1      21       6  160    110  3.9   2.62  16.5     0     1
#>  2 Cluster_1      21       6  160    110  3.9   2.88  17.0     0     1
#>  3 Cluster_2      22.8     4  108     93  3.85  2.32  18.6     1     1
#>  4 Cluster_3      21.4     6  258    110  3.08  3.22  19.4     1     0
#>  5 Cluster_4      18.7     8  360    175  3.15  3.44  17.0     0     0
#>  6 Cluster_3      18.1     6  225    105  2.76  3.46  20.2     1     0
#>  7 Cluster_5      14.3     8  360    245  3.21  3.57  15.8     0     0
#>  8 Cluster_3      24.4     4  147.    62  3.69  3.19  20       1     0
#>  9 Cluster_3      22.8     4  141.    95  3.92  3.15  22.9     1     0
#> 10 Cluster_3      19.2     6  168.   123  3.92  3.44  18.3     1     0
#> # ℹ 22 more rows
#> # ℹ 2 more variables: gear <dbl>, carb <dbl>
```
