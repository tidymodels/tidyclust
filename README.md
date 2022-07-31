
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyclust <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/tidyclust/branch/main/graph/badge.svg)](https://app.codecov.io/gh/EmilHvitfeldt/tidyclust?branch=main)
[![R-CMD-check](https://github.com/EmilHvitfeldt/tidyclust/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EmilHvitfeldt/tidyclust/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of tidyclust is to provide a tidy, unified interface to
clustering models. The packages is closely modeled after the
[parsnip](https://parsnip.tidymodels.org/) package.

## Installation

You can install the development version of tidyclust from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/tidyclust")
```

Please note that this package currently requires a [branch of the
workflows](https://github.com/tidymodels/workflows/tree/tidyclust)
package to work. Use with caution.

## Example

The first thing you do is to create a `cluster specification`. For this
example we are creating a K-means model, using the `stats` engine.

``` r
library(tidyclust)

kmeans_spec <- k_means(num_clusters = 3) %>%
  set_engine("stats") 

kmeans_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 3
#> 
#> Computational engine: stats
```

This specification can then be fit using data.

``` r
kmeans_spec_fit <- kmeans_spec %>%
  fit(~., data = mtcars)
kmeans_spec_fit
#> tidyclust cluster object
#> 
#> K-means clustering with 3 clusters of sizes 9, 16, 7
#> 
#> Cluster means:
#>        mpg      cyl     disp       hp     drat       wt     qsec        vs
#> 1 14.64444 8.000000 388.2222 232.1111 3.343333 4.161556 16.40444 0.0000000
#> 2 24.50000 4.625000 122.2937  96.8750 4.002500 2.518000 18.54312 0.7500000
#> 3 17.01429 7.428571 276.0571 150.7143 2.994286 3.601429 18.11857 0.2857143
#>          am     gear     carb
#> 1 0.2222222 3.444444 4.000000
#> 2 0.6875000 4.125000 2.437500
#> 3 0.0000000 3.000000 2.142857
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   2                   2                   2                   3 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   1                   3                   1                   2 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   2                   2                   2                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   3                   3                   1                   1 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   1                   2                   2                   2 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   2                   3                   3                   1 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   1                   2                   2                   2 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   1                   2                   1                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 46659.32 32838.00 11846.09
#>  (between_SS / total_SS =  85.3 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"
```

Once you have a fitted tidyclust object, you can do a number of things.
`predict()` returns the cluster a new observation belongs to

``` r
predict(kmeans_spec_fit, mtcars[1:4, ])
#> # A tibble: 4 × 1
#>   .pred_cluster
#>   <fct>        
#> 1 Cluster_1    
#> 2 Cluster_1    
#> 3 Cluster_1    
#> 4 Cluster_2
```

`extract_cluster_assignment()` returns the cluster assignments of the
training observations

``` r
extract_cluster_assignment(kmeans_spec_fit)
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
#> # … with 22 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

and `extract_clusters()` returns the locations of the clusters

``` r
extract_centroids(kmeans_spec_fit)
#> # A tibble: 3 × 12
#>   .cluster    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Cluster_1  17.0  7.43  276. 151.   2.99  3.60  18.1 0.286 0      3     2.14
#> 2 Cluster_2  14.6  8     388. 232.   3.34  4.16  16.4 0     0.222  3.44  4   
#> 3 Cluster_3  24.5  4.62  122.  96.9  4.00  2.52  18.5 0.75  0.688  4.12  2.44
```
