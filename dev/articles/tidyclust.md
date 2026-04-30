# Getting started with tidyclust

``` r

library(tidyclust)
```

## Introduction

tidyclust provides a unified, tidy interface to clustering models,
following the same design patterns as
[parsnip](https://parsnip.tidymodels.org/). It lets you swap clustering
algorithms by changing a single line, and integrates seamlessly with the
rest of the tidymodels ecosystem (recipes, workflows, tune).

## The tidyclust workflow

Every tidyclust analysis follows the same four steps:

1.  **Create a model specification** — choose the algorithm and its
    parameters.
2.  **Fit the specification** — train the model on data.
3.  **Extract results** — get cluster assignments, centroids, and
    summaries.
4.  **Evaluate** — use built-in metrics to assess cluster quality.

## K-means example

### 1. Create a specification

``` r

kmeans_spec <- k_means(num_clusters = 3) |>
  set_engine("stats")

kmeans_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 3
#> 
#> Computational engine: stats
```

### 2. Fit to data

``` r

set.seed(1234)
kmeans_fit <- fit(kmeans_spec, ~., data = mtcars)
kmeans_fit
#> tidyclust cluster object
#> 
#> K-means clustering with 3 clusters of sizes 7, 11, 14
#> 
#> Cluster means:
#>        mpg cyl     disp        hp     drat       wt     qsec        vs
#> 1 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286
#> 3 26.66364   4 105.1364  82.63636 4.070909 2.285727 19.13727 0.9090909
#> 2 15.10000   8 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000
#>          am     gear     carb
#> 1 0.4285714 3.857143 3.428571
#> 3 0.7272727 4.090909 1.545455
#> 2 0.1428571 3.285714 3.500000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                   1                   1                   2 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                   1                   3                   1 
#>          Duster 360           Merc 240D            Merc 230 
#>                   3                   2                   2 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                   3                   3                   3 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   3                   3                   2 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   2                   2                   2 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   3                   3                   3 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   3                   2                   2 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   2                   3                   1 
#>       Maserati Bora          Volvo 142E 
#>                   3                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 13954.34 11848.37 93643.90
#>  (between_SS / total_SS =  80.8 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"
```

### 3. Extract results

[`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
returns the cluster label for each training observation:

``` r

extract_cluster_assignment(kmeans_fit)
#> # A tibble: 32 × 1
#>    .cluster 
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
```

[`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
returns the location (mean) of each cluster:

``` r

extract_centroids(kmeans_fit)
#> # A tibble: 3 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  19.7     6  183. 122.   3.59  3.12  18.0 0.571 0.429  3.86
#> 2 Cluster_2  26.7     4  105.  82.6  4.07  2.29  19.1 0.909 0.727  4.09
#> 3 Cluster_3  15.1     8  353. 209.   3.23  4.00  16.8 0     0.143  3.29
#> # ℹ 1 more variable: carb <dbl>
```

[`predict()`](https://rdrr.io/r/stats/predict.html) assigns new
observations to clusters:

``` r

predict(kmeans_fit, new_data = mtcars[1:5, ])
#> # A tibble: 5 × 1
#>   .pred_cluster
#>   <fct>        
#> 1 Cluster_1    
#> 2 Cluster_1    
#> 3 Cluster_2    
#> 4 Cluster_1    
#> 5 Cluster_3
```

[`augment()`](https://generics.r-lib.org/reference/augment.html) appends
the cluster assignment to the original data:

``` r

augment(kmeans_fit, new_data = mtcars)
#> # A tibble: 32 × 12
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 1 more variable: .pred_cluster <fct>
```

### 4. Evaluate

tidyclust provides several cluster quality metrics:

``` r

sse_within_total(kmeans_fit, mtcars)
#> # A tibble: 1 × 3
#>   .metric          .estimator .estimate
#>   <chr>            <chr>          <dbl>
#> 1 sse_within_total standard     119447.
sse_ratio(kmeans_fit, mtcars)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 sse_ratio standard       0.192
silhouette_avg(kmeans_fit, mtcars)
#> # A tibble: 1 × 3
#>   .metric        .estimator .estimate
#>   <chr>          <chr>          <dbl>
#> 1 silhouette_avg standard       0.439
```

Lower
[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
and
[`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md)
indicate tighter clusters. Higher
[`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
(maximum 1) indicates better-separated clusters.

## Hierarchical clustering example

The same workflow applies to
[`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md).
The number of clusters is cut from the dendrogram at fit time using
`num_clusters`:

``` r

hclust_spec <- hier_clust(num_clusters = 3) |>
  set_engine("stats")

hclust_fit <- fit(hclust_spec, ~., data = mtcars)

extract_cluster_assignment(hclust_fit)
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
extract_centroids(hclust_fit)
#> # A tibble: 3 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  24.5  4.62  122.  96.9  4.00  2.52  18.5 0.75  0.688  4.12
#> 2 Cluster_2  17.0  7.43  276. 151.   2.99  3.60  18.1 0.286 0      3   
#> 3 Cluster_3  14.6  8     388. 232.   3.34  4.16  16.4 0     0.222  3.44
#> # ℹ 1 more variable: carb <dbl>
```

## Tidymodels integration

tidyclust works with the broader tidymodels ecosystem. For example, you
can preprocess data with a recipe and bundle it with a model in a
workflow:

``` r

library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
library(workflows)

rec <- recipe(~., data = mtcars) |>
  step_normalize(all_predictors())

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(k_means(num_clusters = 3))

wf_fit <- fit(wf, data = mtcars)
augment(wf_fit, new_data = mtcars)
#> # A tibble: 32 × 12
#>    .pred_cluster   mpg   cyl  disp    hp  drat    wt  qsec    vs    am
#>  * <fct>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Cluster_1      21       6  160    110  3.9   2.62  16.5     0     1
#>  2 Cluster_1      21       6  160    110  3.9   2.88  17.0     0     1
#>  3 Cluster_1      22.8     4  108     93  3.85  2.32  18.6     1     1
#>  4 Cluster_2      21.4     6  258    110  3.08  3.22  19.4     1     0
#>  5 Cluster_3      18.7     8  360    175  3.15  3.44  17.0     0     0
#>  6 Cluster_2      18.1     6  225    105  2.76  3.46  20.2     1     0
#>  7 Cluster_3      14.3     8  360    245  3.21  3.57  15.8     0     0
#>  8 Cluster_2      24.4     4  147.    62  3.69  3.19  20       1     0
#>  9 Cluster_2      22.8     4  141.    95  3.92  3.15  22.9     1     0
#> 10 Cluster_2      19.2     6  168.   123  3.92  3.44  18.3     1     0
#> # ℹ 22 more rows
#> # ℹ 2 more variables: gear <dbl>, carb <dbl>
```

## Next steps

- Learn about tuning the number of clusters in
  `vignette("tuning_and_metrics", package = "tidyclust")`.
- Explore k-means options in
  `vignette("k_means", package = "tidyclust")`.
- Explore hierarchical clustering in
  `vignette("hier_clust", package = "tidyclust")`.
