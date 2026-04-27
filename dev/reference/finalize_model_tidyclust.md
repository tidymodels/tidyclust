# Splice final parameters into objects

**\[deprecated\]**

These functions are deprecated. Please use
[`tune::finalize_model()`](https://tune.tidymodels.org/reference/finalize_model.html)
and
[`tune::finalize_workflow()`](https://tune.tidymodels.org/reference/finalize_model.html)
instead, which now support `cluster_spec` objects natively.

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
best_params <- data.frame(num_clusters = 5)

# Old:
finalize_model_tidyclust(kmeans_spec, best_params)
#> Warning: `finalize_model_tidyclust()` was deprecated in tidyclust 0.3.0.
#> ℹ Please use `tune::finalize_model()` instead.
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 5
#> 
#> Computational engine: stats 
#> 

# New:
tune::finalize_model(kmeans_spec, best_params)
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 5
#> 
#> Computational engine: stats 
#> 
```
