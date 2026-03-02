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
#> 1  19.9  5.71  167. 120.   3.71  3.11  18.5 0.571 0.429   4    3.57
#> 2  27.0  4     102.  81.4  4.09  2.20  18.8 0.9   0.8     4.1  1.5 
#> 3  17.1  7.71  295. 161.   3.05  3.60  17.7 0.143 0       3    2.29
#> 4  14.6  8     340. 272.   3.68  3.54  15.1 0     0.5     4    5   
#> 5  13.7  8     443  206.   3.06  4.97  17.6 0     0       3    3.5 
#> 
#> $n_members
#> [1]  7 10  7  4  4
#> 
#> $sse_within_total_total
#> [1]  8808.032 10247.471 11474.702  7654.146  4665.041
#> 
#> $sse_total
#> [1] 623387.5
#> 
#> $orig_labels
#>  [1] 1 1 2 3 3 1 4 2 1 1 1 3 3 3 5 5 5 2 2 2 2 3 3 4 5 2 2 2 4 1 4 2
#> 
#> $cluster_assignments
#>  [1] Cluster_1 Cluster_1 Cluster_2 Cluster_3 Cluster_3 Cluster_1
#>  [7] Cluster_4 Cluster_2 Cluster_1 Cluster_1 Cluster_1 Cluster_3
#> [13] Cluster_3 Cluster_3 Cluster_5 Cluster_5 Cluster_5 Cluster_2
#> [19] Cluster_2 Cluster_2 Cluster_2 Cluster_3 Cluster_3 Cluster_4
#> [25] Cluster_5 Cluster_2 Cluster_2 Cluster_2 Cluster_4 Cluster_1
#> [31] Cluster_4 Cluster_2
#> Levels: Cluster_1 Cluster_2 Cluster_3 Cluster_4 Cluster_5
#> 
```
