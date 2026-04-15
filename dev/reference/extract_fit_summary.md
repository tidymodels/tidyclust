# S3 method to get fitted model summary info depending on engine

S3 method to get fitted model summary info depending on engine

## Usage

``` r
extract_fit_summary(object, ...)
```

## Arguments

- object:

  a fitted
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object

- ...:

  other arguments passed to methods

## Value

A list with various summary elements

## Details

The elements `cluster_names` and `cluster_assignments` will be factors.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  extract_fit_summary()
#> $cluster_names
#> [1] Cluster_1 Cluster_2 Cluster_3 Cluster_4 Cluster_5
#> Levels: Cluster_1 Cluster_2 Cluster_3 Cluster_4 Cluster_5
#> 
#> $centroids
#> # A tibble: 5 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  24.5  4.62  122.  96.9  4.00  2.52  18.5  0.75 0.688  4.12  2.44
#> 2  19.8  6     242. 108.   2.92  3.34  19.8  1    0      3     1   
#> 3  16.4  8     302. 169.   3.04  3.66  17.4  0    0      3     2.5 
#> 4  14.6  8     340. 272.   3.68  3.54  15.1  0    0.5    4     5   
#> 5  13.7  8     443  206.   3.06  4.97  17.6  0    0      3     3.5 
#> 
#> $n_members
#> [1] 16  2  6  4  4
#> 
#> $sse_within_total_total
#> [1] 32837.9972   562.8304  6815.5541  7654.1463  4665.0415
#> 
#> $sse_total
#> [1] 623387.5
#> 
#> $orig_labels
#>  [1] 1 1 1 2 3 2 4 1 1 1 1 3 3 3 5 5 5 1 1 1 1 3 3 4 5 1 1 1 4 1 4 1
#> 
#> $cluster_assignments
#>  [1] Cluster_1 Cluster_1 Cluster_1 Cluster_2 Cluster_3 Cluster_2
#>  [7] Cluster_4 Cluster_1 Cluster_1 Cluster_1 Cluster_1 Cluster_3
#> [13] Cluster_3 Cluster_3 Cluster_5 Cluster_5 Cluster_5 Cluster_1
#> [19] Cluster_1 Cluster_1 Cluster_1 Cluster_3 Cluster_3 Cluster_4
#> [25] Cluster_5 Cluster_1 Cluster_1 Cluster_1 Cluster_4 Cluster_1
#> [31] Cluster_4 Cluster_1
#> Levels: Cluster_1 Cluster_2 Cluster_3 Cluster_4 Cluster_5
#> 
```
