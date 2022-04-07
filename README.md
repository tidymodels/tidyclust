
<!-- README.md is generated from README.Rmd. Please edit that file -->

# celery

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/celery/branch/main/graph/badge.svg)](https://app.codecov.io/gh/EmilHvitfeldt/celery?branch=main)
[![R-CMD-check](https://github.com/EmilHvitfeldt/celery/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EmilHvitfeldt/celery/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of celery is to provide a tidy, unified interface to clustering
models. The packages is closely modeled after the
[parsnip](https://parsnip.tidymodels.org/) package.

## Installation

You can install the development version of celery from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/celery")
```

Please note that this package currently requires a [branch of the
workflows](https://github.com/tidymodels/workflows/tree/celery) package
to work. Use with caution.

## Example

The first thing you do is to create a `cluster specification`. For this
example we are creating a K-means model, using the `stats` engine.

``` r
library(celery)

kmeans_spec <- k_means(k = 3) %>%
  set_engine_celery("stats") 

kmeans_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   k = 3
#> 
#> Computational engine: stats
```

This specification can then be fit using data.

``` r
kmeans_spec_fit <- kmeans_spec %>%
  fit(~., data = mtcars)
kmeans_spec_fit
#> celery cluster object
#> 
#> K-means clustering with 3 clusters of sizes 14, 11, 7
#> 
#> Cluster means:
#>        mpg cyl     disp        hp     drat       wt     qsec        vs
#> 1 15.10000   8 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000
#> 2 26.66364   4 105.1364  82.63636 4.070909 2.285727 19.13727 0.9090909
#> 3 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286
#>          am     gear     carb
#> 1 0.1428571 3.285714 3.500000
#> 2 0.7272727 4.090909 1.545455
#> 3 0.4285714 3.857143 3.428571
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   3                   3                   2                   3 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   1                   3                   1                   2 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   2                   3                   3                   1 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   1                   1                   1                   1 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   1                   2                   2                   2 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   2                   1                   1                   1 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   1                   2                   2                   2 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   1                   3                   1                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 93643.90 11848.37 13954.34
#>  (between_SS / total_SS =  80.8 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"
```

Once you have a fitted celery object, you can do a number of things.
`predict()` returns the cluster a new observation belongs to

``` r
predict(kmeans_spec_fit, mtcars[1:4, ])
#> # A tibble: 4 × 1
#>   .pred_cluster
#>   <fct>        
#> 1 3            
#> 2 3            
#> 3 2            
#> 4 3
```

`extract_cluster_assignment()` returns the cluster assignments of the
training observations

``` r
extract_cluster_assignment(kmeans_spec_fit)
#> # A tibble: 32 × 1
#>    .cluster
#>    <fct>   
#>  1 C1      
#>  2 C1      
#>  3 C2      
#>  4 C1      
#>  5 C3      
#>  6 C1      
#>  7 C3      
#>  8 C2      
#>  9 C2      
#> 10 C1      
#> # … with 22 more rows
```

and `extract_clusters()` returns the locations of the clusters

``` r
extract_centroids(kmeans_spec_fit)
#> New names:
#> • `` -> `...2`
#> # A tibble: 3 × 2
#>   .cluster   ...2
#>   <chr>     <dbl>
#> 1 Cluster_1  19.7
#> 2 Cluster_2  26.7
#> 3 Cluster_3  15.1
```
