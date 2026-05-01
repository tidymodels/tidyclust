# Axing a cluster_fit.

cluster_fit objects are created from the tidyclust package.

## Usage

``` r
axe_call.cluster_fit(x, verbose = FALSE, ...)

axe_ctrl.cluster_fit(x, verbose = FALSE, ...)

axe_data.cluster_fit(x, verbose = FALSE, ...)

axe_env.cluster_fit(x, verbose = FALSE, ...)

axe_fitted.cluster_fit(x, verbose = FALSE, ...)
```

## Arguments

- x:

  A model object.

- verbose:

  Print information each time an axe method is executed. Notes how much
  memory is released and what functions are disabled. Default is
  `FALSE`.

- ...:

  Any additional arguments related to axing.

## Value

Axed cluster_fit object.

## Examples

``` r
k_fit <- k_means(num_clusters = 3) |>
  parsnip::set_engine("stats") |>
  fit(~., data = mtcars)

butcher::butcher(k_fit)
#> tidyclust cluster object
#> 
#> K-means clustering with 3 clusters of sizes 7, 11, 14
#> 
#> Cluster means:
#>        mpg cyl     disp        hp     drat       wt     qsec        vs
#> 1 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286
#> 2 26.66364   4 105.1364  82.63636 4.070909 2.285727 19.13727 0.9090909
#> 3 15.10000   8 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000
#>          am     gear     carb
#> 1 0.4285714 3.857143 3.428571
#> 2 0.7272727 4.090909 1.545455
#> 3 0.1428571 3.285714 3.500000
#> 
#> Clustering vector:
#> integer(0)
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
