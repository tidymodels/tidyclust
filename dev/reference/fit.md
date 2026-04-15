# Fit a Model Specification to a Data Set

[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) take a
model specification, translate_tidyclust the required code by
substituting arguments, and execute the model fit routine.

## Usage

``` r
# S3 method for class 'cluster_spec'
fit(object, formula, data, control = control_cluster(), ...)

# S3 method for class 'cluster_spec'
fit_xy(object, x, case_weights = NULL, control = control_cluster(), ...)
```

## Arguments

- object:

  An object of class
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  that has a chosen engine (via
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)).

- formula:

  An object of class `formula` (or one that can be coerced to that
  class): a symbolic description of the model to be fitted.

- data:

  Optional, depending on the interface (see Details below). A data frame
  containing all relevant variables (e.g. predictors, case weights,
  etc). Note: when needed, a *named argument* should be used.

- control:

  A named list with elements `verbosity` and `catch`. See
  [`control_cluster()`](https://tidyclust.tidymodels.org/dev/reference/control_cluster.md).

- ...:

  Not currently used; values passed here will be ignored. Other options
  required to fit the model should be passed using
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

- x:

  A matrix, sparse matrix, or data frame of predictors. Only some models
  have support for sparse matrix input. See
  [`modelenv::get_encoding()`](https://modelenv.tidymodels.org/reference/set_encoding.html)
  for details. `x` should have column names.

- case_weights:

  An optional classed vector of numeric case weights. This must return
  `TRUE` when
  [`hardhat::is_case_weights()`](https://hardhat.tidymodels.org/reference/is_case_weights.html)
  is run on it. See
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)
  and
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
  for examples.

## Value

A
[`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
object that contains several elements:

- `spec`: The model specification object (`object` in the call to `fit`)

- `fit`: when the model is executed without error, this is the model
  object. Otherwise, it is a `try-error` object with the error message.

- `preproc`: any objects needed to convert between a formula and
  non-formula interface (such as the `terms` object)

The return value will also have a class related to the fitted model
(e.g. `"_kmeans"`) before the base class of `"cluster_fit"`.

A fitted
[`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
object.

## Details

[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
substitute the current arguments in the model specification into the
computational engine's code, check them for validity, then fit the model
using the data and the engine-specific code. Different model functions
have different interfaces (e.g. formula or `x`/`y`) and these functions
translate_tidyclust between the interface used when
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) was
invoked and the one required by the underlying model.

When possible, these functions attempt to avoid making copies of the
data. For example, if the underlying model uses a formula and
[`fit()`](https://generics.r-lib.org/reference/fit.html) is invoked, the
original data are references when the model is fit. However, if the
underlying model uses something else, such as `x`/`y`, the formula is
evaluated and the data are converted to the required format. In this
case, any calls in the resulting model objects reference the temporary
objects used to fit the model.

If the model engine has not been set, the model's default engine will be
used (as discussed on each model page). If the `verbosity` option of
[`control_cluster()`](https://tidyclust.tidymodels.org/dev/reference/control_cluster.md)
is greater than zero, a warning will be produced.

If you would like to use an alternative method for generating contrasts
when supplying a formula to
[`fit()`](https://generics.r-lib.org/reference/fit.html), set the global
option `contrasts` to your preferred method. For example, you might set
it to:
`options(contrasts = c(unordered = "contr.helmert", ordered = "contr.poly"))`.
See the help page for
[`stats::contr.treatment()`](https://rdrr.io/r/stats/contrast.html) for
more possible contrast types.

## See also

[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html),
[`control_cluster()`](https://tidyclust.tidymodels.org/dev/reference/control_cluster.md),
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md),
[`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

kmeans_mod <- k_means(num_clusters = 5)

using_formula <-
  kmeans_mod |>
  set_engine("stats") |>
  fit(~., data = mtcars)

using_x <-
  kmeans_mod |>
  set_engine("stats") |>
  fit_xy(x = mtcars)

using_formula
#> tidyclust cluster object
#> 
#> K-means clustering with 5 clusters of sizes 7, 7, 10, 4, 4
#> 
#> Cluster means:
#>        mpg cyl     disp        hp     drat       wt     qsec        vs
#> 2 19.74286   6 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286
#> 1 24.18571   4 121.7143  94.28571 3.924286 2.508286 19.10286 0.8571429
#> 4 15.67000   8 317.1400 210.40000 3.297000 3.612500 16.45400 0.0000000
#> 3 13.67500   8 443.0000 206.25000 3.060000 4.966000 17.56750 0.0000000
#> 5 31.00000   4  76.1250  62.25000 4.327500 1.896250 19.19750 1.0000000
#>          am     gear     carb
#> 2 0.4285714 3.857143 3.428571
#> 1 0.5714286 4.142857 1.714286
#> 4 0.2000000 3.400000 3.500000
#> 3 0.0000000 3.000000 3.500000
#> 5 1.0000000 4.000000 1.250000
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
#>                   3                   3                   4 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   4                   4                   5 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   5                   5                   2 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   3                   3                   3 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   4                   5                   2 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   2                   3                   1 
#>       Maserati Bora          Volvo 142E 
#>                   3                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 13954.3363  3616.8297 43649.5192  4665.0415   208.0365
#>  (between_SS / total_SS =  89.4 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"      
using_x
#> tidyclust cluster object
#> 
#> K-means clustering with 5 clusters of sizes 16, 2, 6, 4, 4
#> 
#> Cluster means:
#>        mpg   cyl     disp       hp   drat     wt     qsec   vs     am
#> 2 24.50000 4.625 122.2937  96.8750 4.0025 2.5180 18.54312 0.75 0.6875
#> 4 19.75000 6.000 241.5000 107.5000 2.9200 3.3375 19.83000 1.00 0.0000
#> 1 16.38333 8.000 301.5667 169.1667 3.0450 3.6625 17.36500 0.00 0.0000
#> 5 14.60000 8.000 340.5000 272.2500 3.6750 3.5375 15.08750 0.00 0.5000
#> 3 13.67500 8.000 443.0000 206.2500 3.0600 4.9660 17.56750 0.00 0.0000
#>    gear   carb
#> 2 4.125 2.4375
#> 4 3.000 1.0000
#> 1 3.000 2.5000
#> 5 4.000 5.0000
#> 3 3.000 3.5000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                   1                   1                   1 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                   2                   3                   2 
#>          Duster 360           Merc 240D            Merc 230 
#>                   4                   1                   1 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                   3                   3                   5 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   5                   5                   1 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   1                   1                   1 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   3                   3                   4 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   5                   1                   1 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   1                   4                   1 
#>       Maserati Bora          Volvo 142E 
#>                   4                   1 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 32837.9972   562.8304  6815.5541  7654.1463  4665.0415
#>  (between_SS / total_SS =  91.6 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"      
```
