# Splice final parameters into objects

The `finalize_*` functions take a list or tibble of tuning parameter
values and update objects with those values.

## Usage

``` r
finalize_model_tidyclust(x, parameters)

finalize_workflow_tidyclust(x, parameters)
```

## Arguments

- x:

  A recipe, `parsnip` model specification, or workflow.

- parameters:

  A list or 1-row tibble of parameter values. Note that the column names
  of the tibble should be the `id` fields attached to
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html). For
  example, in the `Examples` section below, the model has `tune("K")`.
  In this case, the parameter tibble should be "K" and not "neighbors".

## Value

An updated version of `x`.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = tune())
kmeans_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = tune()
#> 
#> Computational engine: stats 
#> 

best_params <- data.frame(num_clusters = 5)
best_params
#>   num_clusters
#> 1            5

finalize_model_tidyclust(kmeans_spec, best_params)
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 5
#> 
#> Computational engine: stats 
#> 
```
