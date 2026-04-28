# Model tuning via grid search

`tune_cluster()` computes a set of performance metrics for a pre-defined
set of tuning parameters that correspond to a cluster model or recipe
across one or more resamples of the data.

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
`.notes` contains warnings and errors that occur during execution. The
`.notes` column is a tibble with columns `location`, `type`, `note`, and
`trace`. The `trace` column contains
[`rlang::trace_back()`](https://rlang.r-lib.org/reference/trace_back.html)
objects for errors and warnings, which can be useful for debugging.

## Choosing metrics

The `metrics` argument accepts a
[`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md).
If `NULL`, the default metrics are
[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
and
[`sse_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md).

Common metrics and their interpretation:

- [`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md):
  Total within-cluster sum of squares. Lower values indicate tighter,
  more compact clusters. Use the "elbow method" — plot this against
  `num_clusters` and look for where the improvement flattens.

- [`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md):
  Ratio of within-cluster SS to total SS. Lower is better (more variance
  explained by the clustering).

- [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md):
  Average silhouette width (range -1 to 1). Higher values indicate
  better-separated clusters. Values above 0.5 are generally considered
  good.

After tuning, use these functions to inspect results:

- [`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html):
  All metrics for every parameter combination.

- [`tune::show_best()`](https://tune.tidymodels.org/reference/show_best.html):
  Top N parameter combinations for a given metric.

- [`tune::select_best()`](https://tune.tidymodels.org/reference/show_best.html):
  Single best parameter combination.

## Configuration column

The `.config` column in the results follows the pattern
`pre{num}_mod{num}_post{num}`. The numbers encode which combination of
preprocessor, model, and postprocessor parameters was used. A value of
`0` means that element was not tuned. For example, `pre0_mod2_post0`
means the preprocessor was not tuned and this is the second model
parameter combination.

## Parallel processing

Parallel processing is supported via the `future` and `mirai` packages.
To enable parallelism, set up a `future` plan or `mirai` daemons before
calling `tune_cluster()`:

    # Using future
    library(future)
    plan(multisession, workers = 4)
    res <- tune_cluster(wflow, resamples = folds, grid = grid)
    plan(sequential)

    # Using mirai
    library(mirai)
    daemons(4)
    res <- tune_cluster(wflow, resamples = folds, grid = grid)
    daemons(0)

See
[tune::parallelism](https://tune.tidymodels.org/reference/parallelism.html)
for more details.

## Examples

``` r
library(recipes)
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
#> 1 <split [16/16]> Fold1 <tibble [6 × 5]> <tibble [0 × 4]>
#> 2 <split [16/16]> Fold2 <tibble [6 × 5]> <tibble [0 × 4]>

collect_metrics(res)
#> # A tibble: 6 × 7
#>   num_clusters .metric          .estimator  mean     n std_err .config 
#>          <int> <chr>            <chr>      <dbl> <int>   <dbl> <chr>   
#> 1            1 sse_total        standard   167.      2    7.78 pre0_mo…
#> 2            1 sse_within_total standard   167.      2    7.78 pre0_mo…
#> 3            2 sse_total        standard   167.      2    7.78 pre0_mo…
#> 4            2 sse_within_total standard    82.2     2    5.76 pre0_mo…
#> 5            3 sse_total        standard   167.      2    7.78 pre0_mo…
#> 6            3 sse_within_total standard    71.5     2    2.59 pre0_mo…
```
