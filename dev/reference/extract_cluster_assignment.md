# Extract cluster assignments from model

When applied to a fitted cluster specification, returns a tibble with
cluster assignments of the data used to train the model.

## Usage

``` r
extract_cluster_assignment(object, ...)
```

## Arguments

- object:

  An fitted
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object.

- ...:

  Other arguments passed to methods. Using the `prefix` allows you to
  change the prefix in the levels of the factor levels. Using `labels`
  allows you to provide a character vector of cluster labels, overriding
  `prefix`.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 1 column named `.cluster`. This tibble will correspond the the
training data set.

## Details

Some model types such as K-means as seen in
[`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
stores the cluster assignments in the object itself. leading the use of
this function to act as an simple extract. Other model types such as
Hierarchical (Agglomerative) Clustering as seen in
[`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md),
are fit in such a way that the number of clusters can be determined at
any time after the fit. Setting the `num_clusters` or `cut_height` in
this function will be used to determine the clustering when reported.

The ordering of the clusters is such that the first observation in the
training data set will be in cluster 1, the next observation that
doesn't belong to cluster 1 will be in cluster 2, and so on and forth.
As the ordering of clustering doesn't matter, this is done to avoid
identical sets of clustering having different labels if fit multiple
times.

### Related functions

`extract_cluster_assignment()` is a part of a trio of functions doing
similar things:

- `extract_cluster_assignment()` returns the cluster assignments of the
  training observations

- [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
  returns the location of the centroids

- [`predict()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)
  returns the cluster a new observation belongs to

## See also

[`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
[`predict.cluster_fit()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  extract_cluster_assignment()
#> # A tibble: 32 × 1
#>    .cluster 
#>    <fct>    
#>  1 Cluster_1
#>  2 Cluster_1
#>  3 Cluster_1
#>  4 Cluster_2
#>  5 Cluster_3
#>  6 Cluster_2
#>  7 Cluster_4
#>  8 Cluster_1
#>  9 Cluster_1
#> 10 Cluster_1
#> # ℹ 22 more rows

kmeans_fit |>
  extract_cluster_assignment(prefix = "C_")
#> # A tibble: 32 × 1
#>    .cluster
#>    <fct>   
#>  1 C_1     
#>  2 C_1     
#>  3 C_1     
#>  4 C_2     
#>  5 C_3     
#>  6 C_2     
#>  7 C_4     
#>  8 C_1     
#>  9 C_1     
#> 10 C_1     
#> # ℹ 22 more rows

kmeans_fit |>
  extract_cluster_assignment(labels = c("A", "B", "C", "D", "E"))
#> # A tibble: 32 × 1
#>    .cluster
#>    <fct>   
#>  1 A       
#>  2 A       
#>  3 A       
#>  4 B       
#>  5 C       
#>  6 B       
#>  7 D       
#>  8 A       
#>  9 A       
#> 10 A       
#> # ℹ 22 more rows

# Some models such as `hier_clust()` fits in such a way that you can specify
# the number of clusters after the model is fit
hclust_spec <- hier_clust() |>
  set_engine("stats")

hclust_fit <- fit(hclust_spec, ~., mtcars)

hclust_fit |>
  extract_cluster_assignment(num_clusters = 2)
#> # A tibble: 32 × 1
#>    .cluster 
#>    <fct>    
#>  1 Cluster_1
#>  2 Cluster_1
#>  3 Cluster_1
#>  4 Cluster_1
#>  5 Cluster_2
#>  6 Cluster_1
#>  7 Cluster_2
#>  8 Cluster_1
#>  9 Cluster_1
#> 10 Cluster_1
#> # ℹ 22 more rows

hclust_fit |>
  extract_cluster_assignment(cut_height = 250)
#> # A tibble: 32 × 1
#>    .cluster 
#>    <fct>    
#>  1 Cluster_1
#>  2 Cluster_1
#>  3 Cluster_1
#>  4 Cluster_2
#>  5 Cluster_3
#>  6 Cluster_2
#>  7 Cluster_3
#>  8 Cluster_1
#>  9 Cluster_1
#> 10 Cluster_1
#> # ℹ 22 more rows
```
