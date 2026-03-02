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
#> K-means clustering with 5 clusters of sizes 7, 9, 6, 9, 1
#> 
#> Cluster means:
#>        mpg      cyl      disp        hp     drat       wt     qsec
#> 5 20.61429 5.428571 166.81429 104.00000 3.715714 3.167857 19.11429
#> 4 27.34444 4.000000  96.55556  83.55556 4.130000 2.089222 18.62333
#> 2 16.83333 7.666667 284.56667 158.33333 3.033333 3.625000 17.76833
#> 3 14.64444 8.000000 388.22222 232.11111 3.343333 4.161556 16.40444
#> 1 19.70000 6.000000 145.00000 175.00000 3.620000 2.770000 15.50000
#>          vs        am     gear     carb
#> 5 0.7142857 0.2857143 3.857143 3.000000
#> 4 0.8888889 0.8888889 4.111111 1.444444
#> 2 0.1666667 0.0000000 3.000000 2.333333
#> 3 0.0000000 0.2222222 3.444444 4.000000
#> 1 0.0000000 1.0000000 5.000000 6.000000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                   1                   1                   2 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                   3                   4                   1 
#>          Duster 360           Merc 240D            Merc 230 
#>                   4                   1                   1 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                   3                   3                   4 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   4                   4                   2 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   2                   2                   2 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   3                   3                   4 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   4                   2                   2 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   2                   4                   5 
#>       Maserati Bora          Volvo 142E 
#>                   4                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1]  7286.292  7554.490  6355.581 46659.317     0.000
#>  (between_SS / total_SS =  89.1 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"      
using_x
#> tidyclust cluster object
#> 
#> K-means clustering with 5 clusters of sizes 6, 7, 6, 9, 4
#> 
#> Cluster means:
#>        mpg      cyl     disp        hp     drat       wt     qsec
#> 3 19.46667 6.000000 170.8667 124.33333 3.670000 3.100833 17.73333
#> 5 24.18571 4.000000 121.7143  94.28571 3.924286 2.508286 19.10286
#> 4 16.83333 7.666667 284.5667 158.33333 3.033333 3.625000 17.76833
#> 2 14.64444 8.000000 388.2222 232.11111 3.343333 4.161556 16.40444
#> 1 31.00000 4.000000  76.1250  62.25000 4.327500 1.896250 19.19750
#>          vs        am     gear     carb
#> 3 0.5000000 0.5000000 4.000000 3.833333
#> 5 0.8571429 0.5714286 4.142857 1.714286
#> 4 0.1666667 0.0000000 3.000000 2.333333
#> 2 0.0000000 0.2222222 3.444444 4.000000
#> 1 1.0000000 1.0000000 4.000000 1.250000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                   1                   1                   2 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                   3                   4                   1 
#>          Duster 360           Merc 240D            Merc 230 
#>                   4                   2                   2 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                   3                   3                   4 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   4                   4                   5 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   5                   5                   2 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   3                   3                   4 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   4                   5                   2 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   2                   4                   1 
#>       Maserati Bora          Volvo 142E 
#>                   4                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1]  7256.4492  3616.8297  6355.5813 46659.3172   208.0365
#>  (between_SS / total_SS =  89.7 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"      
```
