
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

## Example

The first thing you do is to create a `cluster specification`. For this
example we are creating a K-means model, using the `stats` engine.

``` r
library(celery)

k_means_spec <- k_means(k = 3) %>%
  set_engine_celery("stats") 

k_means_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   k = 3
#> 
#> Computational engine: stats
```

This specification can then be fit using data.

``` r
k_means_spec %>%
  fit(~., data = mtcars)
#> $spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   k = 3
#> 
#> Computational engine: stats 
#> 
#> Model fit template:
#> stats::kmeans(x = missing_arg(), centers = missing_arg(), centers = 3)
#> 
#> $fit
#> K-means clustering with 3 clusters of sizes 4, 17, 11
#> 
#> Cluster means:
#>        mpg      cyl     disp        hp     drat       wt     qsec         vs
#> 1 13.67500 8.000000 443.0000 206.25000 3.060000 4.966000 17.56750 0.00000000
#> 2 24.12353 4.705882 128.3353  97.35294 3.929412 2.573412 18.64176 0.76470588
#> 3 16.19091 7.818182 311.7636 201.27273 3.277273 3.576364 16.72545 0.09090909
#>          am     gear     carb
#> 1 0.0000000 3.000000 3.500000
#> 2 0.6470588 4.058824 2.352941
#> 3 0.1818182 3.363636 3.272727
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   2                   2                   2                   3 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   3                   2                   3                   2 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   2                   2                   2                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   3                   3                   1                   1 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   1                   2                   2                   2 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   2                   3                   3                   3 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   1                   2                   2                   2 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   3                   2                   3                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1]  4665.041 42877.103 56041.432
#>  (between_SS / total_SS =  83.4 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"      
#> 
#> $elapsed
#> $elapsed$elapsed
#> [1] NA
#> 
#> 
#> $preproc
#> $preproc$offset
#> NULL
#> 
#> $preproc$terms
#> ~mpg + cyl + disp + hp + drat + wt + qsec + vs + am + gear + 
#>     carb
#> attr(,"variables")
#> list(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
#> attr(,"factors")
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg    1   0    0  0    0  0    0  0  0    0    0
#> cyl    0   1    0  0    0  0    0  0  0    0    0
#> disp   0   0    1  0    0  0    0  0  0    0    0
#> hp     0   0    0  1    0  0    0  0  0    0    0
#> drat   0   0    0  0    1  0    0  0  0    0    0
#> wt     0   0    0  0    0  1    0  0  0    0    0
#> qsec   0   0    0  0    0  0    1  0  0    0    0
#> vs     0   0    0  0    0  0    0  1  0    0    0
#> am     0   0    0  0    0  0    0  0  1    0    0
#> gear   0   0    0  0    0  0    0  0  0    1    0
#> carb   0   0    0  0    0  0    0  0  0    0    1
#> attr(,"term.labels")
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
#> [11] "carb"
#> attr(,"order")
#>  [1] 1 1 1 1 1 1 1 1 1 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 0
#> attr(,".Environment")
#> <environment: 0x1130df7e8>
#> attr(,"predvars")
#> list(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
#> attr(,"dataClasses")
#>       mpg       cyl      disp        hp      drat        wt      qsec        vs 
#> "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" 
#>        am      gear      carb 
#> "numeric" "numeric" "numeric" 
#> 
#> $preproc$xlevels
#> named list()
#> 
#> $preproc$options
#> $preproc$options$indicators
#> [1] "traditional"
#> 
#> $preproc$options$composition
#> [1] "matrix"
#> 
#> $preproc$options$remove_intercept
#> [1] TRUE
#> 
#> 
#> 
#> attr(,"class")
#> [1] "_kmeans"     "cluster_fit"
```
