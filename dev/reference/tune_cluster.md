# Model tuning via grid search

`tune_cluster()` computes a set of performance metrics (e.g. accuracy or
RMSE) for a pre-defined set of tuning parameters that correspond to a
model or recipe across one or more resamples of the data.

## Usage

``` r
tune_cluster(object, ...)

# S3 method for class 'cluster_spec'
tune_cluster(
  object,
  preprocessor,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = tune::control_grid()
)

# S3 method for class 'workflow'
tune_cluster(
  object,
  resamples,
  ...,
  param_info = NULL,
  grid = 10,
  metrics = NULL,
  control = tune::control_grid()
)
```

## Arguments

- object:

  A `tidyclust` model specification or a
  [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html).

- ...:

  Not currently used.

- preprocessor:

  A traditional model formula or a recipe created using
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).

- resamples:

  An `rset()` object.

- param_info:

  A
  [`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  object or `NULL`. If none is given, a parameters set is derived from
  other arguments. Passing this argument can be useful when parameter
  ranges need to be customized.

- grid:

  A data frame of tuning combinations or a positive integer. The data
  frame should have columns for each parameter being tuned and rows for
  tuning parameter candidates. An integer denotes the number of
  candidate parameter sets to be created automatically.

- metrics:

  A
  [`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md)
  or `NULL`.

- control:

  An object used to modify the tuning process. Defaults to
  [`tune::control_grid()`](https://tune.tidymodels.org/reference/control_grid.html).

## Value

An updated version of `resamples` with extra list columns for `.metrics`
and `.notes` (optional columns are `.predictions` and `.extracts`).
`.notes` contains warnings and errors that occur during execution.

## Examples

``` r
library(recipes)
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
library(rsample)
library(workflows)
library(tune)

rec_spec <- recipe(~., data = mtcars) |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors())

kmeans_spec <- k_means(num_clusters = tune())

wflow <- workflow() |>
  add_recipe(rec_spec) |>
  add_model(kmeans_spec)

grid <- tibble(num_clusters = 1:3)

set.seed(4400)
folds <- vfold_cv(mtcars, v = 2)

res <- tune_cluster(
  wflow,
  resamples = folds,
  grid = grid
)
res
#> # Tuning results
#> # 2-fold cross-validation 
#> # A tibble: 2 × 4
#>   splits          id    .metrics         .notes          
#>   <list>          <chr> <list>           <list>          
#> 1 <split [16/16]> Fold1 <tibble [6 × 5]> <tibble [0 × 3]>
#> 2 <split [16/16]> Fold2 <tibble [6 × 5]> <tibble [0 × 3]>

collect_metrics(res)
#> # A tibble: 6 × 7
#>   num_clusters .metric          .estimator  mean     n std_err .config 
#>          <int> <chr>            <chr>      <dbl> <int>   <dbl> <chr>   
#> 1            1 sse_total        standard   160.      2   0.346 Preproc…
#> 2            1 sse_within_total standard   160.      2   0.346 Preproc…
#> 3            2 sse_total        standard   160.      2   0.346 Preproc…
#> 4            2 sse_within_total standard    80.3     2   3.63  Preproc…
#> 5            3 sse_total        standard   160.      2   0.346 Preproc…
#> 6            3 sse_within_total standard    54.3     2   7.15  Preproc…
```
