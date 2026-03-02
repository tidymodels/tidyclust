# Model predictions

Apply to a model to create different types of predictions.
[`predict()`](https://rdrr.io/r/stats/predict.html) can be used for all
types of models and uses the "type" argument for more specificity.

## Usage

``` r
# S3 method for class 'cluster_fit'
predict(object, new_data, type = NULL, opts = list(), ...)

# S3 method for class 'cluster_fit'
predict_raw(object, new_data, opts = list(), ...)
```

## Arguments

- object:

  An object of class
  [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md).

- new_data:

  A rectangular data object, such as a data frame.

- type:

  A single character value or `NULL`. Possible values are "cluster", or
  "raw". When `NULL`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) will choose an
  appropriate value based on the model's mode.

- opts:

  A list of optional arguments to the underlying predict function that
  will be used when `type = "raw"`. The list should not include options
  for the model object or the new data being predicted.

- ...:

  Arguments to the underlying model's prediction function cannot be
  passed here (see `opts`).

## Value

With the exception of `type = "raw"`, the results of
`predict.cluster_fit()` will be a tibble as many rows in the output as
there are rows in `new_data` and the column names will be predictable.

For clustering results the tibble will have a `.pred_cluster` column.

Using `type = "raw"` with `predict.cluster_fit()` will return the
unadulterated results of the prediction function.

When the model fit failed and the error was captured, the
[`predict()`](https://rdrr.io/r/stats/predict.html) function will return
the same structure as above but filled with missing values. This does
not currently work for multivariate models.

## Details

If "type" is not supplied to
[`predict()`](https://rdrr.io/r/stats/predict.html), then a choice is
made:

- `type = "cluster"` for clustering models

[`predict()`](https://rdrr.io/r/stats/predict.html) is designed to
provide a tidy result (see "Value" section below) in a tibble output
format.

The ordering of the clusters is such that the first observation in the
training data set will be in cluster 1, the next observation that
doesn't belong to cluster 1 will be in cluster 2, and so on and forth.
As the ordering of clustering doesn't matter, this is done to avoid
identical sets of clustering having different labels if fit multiple
times.

### What does it mean to predict?

Prediction is not always formally defined for clustering models.
Therefore, each
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
method will have their own section on how "prediction" is interpreted,
and done if implemented.

### Related functions

[`predict()`](https://rdrr.io/r/stats/predict.html) when used with
tidyclust objects is a part of a trio of functions doing similar things:

- [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  returns the cluster assignments of the training observations

- [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
  returns the location of the centroids

- [`predict()`](https://rdrr.io/r/stats/predict.html) returns the
  cluster a new observation belongs to

## See also

[`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
[`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  predict(new_data = mtcars)
#> # A tibble: 32 × 1
#>    .pred_cluster
#>    <fct>        
#>  1 Cluster_1    
#>  2 Cluster_1    
#>  3 Cluster_2    
#>  4 Cluster_1    
#>  5 Cluster_3    
#>  6 Cluster_1    
#>  7 Cluster_3    
#>  8 Cluster_2    
#>  9 Cluster_2    
#> 10 Cluster_1    
#> # ℹ 22 more rows

# Some models such as `hier_clust()` fits in such a way that you can specify
# the number of clusters after the model is fit
hclust_spec <- hier_clust() |>
  set_engine("stats")

hclust_fit <- fit(hclust_spec, ~., mtcars)

hclust_fit |>
  predict(new_data = mtcars[4:6, ], num_clusters = 2)
#> # A tibble: 3 × 1
#>   .pred_cluster
#>   <fct>        
#> 1 Cluster_1    
#> 2 Cluster_2    
#> 3 Cluster_1    

hclust_fit |>
  predict(new_data = mtcars[4:6, ], cut_height = 250)
#> # A tibble: 3 × 1
#>   .pred_cluster
#>   <fct>        
#> 1 Cluster_2    
#> 2 Cluster_2    
#> 3 Cluster_2    
```
