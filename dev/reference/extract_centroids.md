# Extract clusters from model

When applied to a fitted cluster specification, returns a tibble with
cluster location. When such locations doesn't make sense for the model,
a mean location is used.

## Usage

``` r
extract_centroids(object, ...)
```

## Arguments

- object:

  An fitted
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object.

- ...:

  Other arguments passed to methods. Using the `prefix` allows you to
  change the prefix in the levels of the factor levels.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 1 row for each centroid and their position. `.cluster` denotes the
cluster name for the centroid. The remaining variables match variables
passed into model.

## Details

Some model types such as K-means as seen in
[`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
stores the centroid in the object itself. leading the use of this
function to act as an simple extract. Other model types such as
Hierarchical (Agglomerative) Clustering as seen in
[`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md),
are fit in such a way that the number of clusters can be determined at
any time after the fit. Setting the `num_clusters` or `cut_height` in
this function will be used to determine the clustering when reported.

Further more, some models like
[`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md),
doesn't have a notion of "centroids". The mean of the observation within
each cluster assignment is returned as the centroid.

The ordering of the clusters is such that the first observation in the
training data set will be in cluster 1, the next observation that
doesn't belong to cluster 1 will be in cluster 2, and so on and forth.
As the ordering of clustering doesn't matter, this is done to avoid
identical sets of clustering having different labels if fit multiple
times.

### Related functions

`extract_centroids()` is a part of a trio of functions doing similar
things:

- [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  returns the cluster assignments of the training observations

- `extract_centroids()` returns the location of the centroids

- [`predict()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)
  returns the cluster a new observation belongs to

## See also

[`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
[`predict.cluster_fit()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)

## Examples

``` r
set.seed(1234)
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  extract_centroids()
#> # A tibble: 5 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  19.9  5.71  167. 120.   3.71  3.11  18.5 0.571 0.429   4  
#> 2 Cluster_2  27.0  4     102.  81.4  4.09  2.20  18.8 0.9   0.8     4.1
#> 3 Cluster_3  17.1  7.71  295. 161.   3.05  3.60  17.7 0.143 0       3  
#> 4 Cluster_4  14.6  8     340. 272.   3.68  3.54  15.1 0     0.5     4  
#> 5 Cluster_5  13.7  8     443  206.   3.06  4.97  17.6 0     0       3  
#> # ℹ 1 more variable: carb <dbl>

# Some models such as `hier_clust()` fits in such a way that you can specify
# the number of clusters after the model is fit.
# A Hierarchical (Agglomerative) Clustering method doesn't technically have
# clusters, so the center of the observation within each cluster is returned
# instead.
hclust_spec <- hier_clust() |>
  set_engine("stats")

hclust_fit <- fit(hclust_spec, ~., mtcars)

hclust_fit |>
  extract_centroids(num_clusters = 2)
#> # A tibble: 2 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  22.2  5.48  169.  113.  3.70  2.85  18.4 0.609 0.478  3.78
#> 2 Cluster_2  14.6  8     388.  232.  3.34  4.16  16.4 0     0.222  3.44
#> # ℹ 1 more variable: carb <dbl>

hclust_fit |>
  extract_centroids(cut_height = 250)
#> # A tibble: 3 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  24.5  4.62  122.  96.9  4.00  2.52  18.5 0.75  0.688  4.12
#> 2 Cluster_2  17.0  7.43  276. 151.   2.99  3.60  18.1 0.286 0      3   
#> 3 Cluster_3  14.6  8     388. 232.   3.34  4.16  16.4 0     0.222  3.44
#> # ℹ 1 more variable: carb <dbl>
```
