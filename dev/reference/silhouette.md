# Measures silhouette between clusters

Measures silhouette between clusters

## Usage

``` r
silhouette(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance
)
```

## Arguments

- object:

  A fitted tidyclust model

- new_data:

  A dataset to predict on. If `NULL`, uses trained clustering.

- dists:

  A distance matrix. Used if `new_data` is `NULL`.

- dist_fun:

  A function for calculating distances between observations. Defaults to
  Euclidean distance on processed data.

## Value

A tibble giving the silhouette for each observation.

## Details

[`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
is the corresponding cluster metric function that returns the average of
the values given by `silhouette()`.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

dists <- mtcars |>
  as.matrix() |>
  dist()

silhouette(kmeans_fit, dists = dists)
#> # A tibble: 32 × 3
#>    cluster   neighbor  sil_width
#>    <fct>     <fct>         <dbl>
#>  1 Cluster_1 Cluster_5    0.569 
#>  2 Cluster_1 Cluster_5    0.570 
#>  3 Cluster_2 Cluster_1    0.501 
#>  4 Cluster_3 Cluster_1    0.246 
#>  5 Cluster_4 Cluster_3   -0.153 
#>  6 Cluster_1 Cluster_3    0.148 
#>  7 Cluster_4 Cluster_3    0.365 
#>  8 Cluster_1 Cluster_2    0.0271
#>  9 Cluster_1 Cluster_2    0.162 
#> 10 Cluster_1 Cluster_5    0.424 
#> # ℹ 22 more rows
```
