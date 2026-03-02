# Extract elements of a tidyclust model object

These functions extract various elements from a clustering object. If
they do not exist yet, an error is thrown.

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the engine specific fit embedded within a tidyclust model fit.
  For example, when using
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  with the `"lm"` engine, this returns the underlying `kmeans` object.

- [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a set of dials parameter objects.

## Usage

``` r
# S3 method for class 'cluster_fit'
extract_fit_engine(x, ...)

# S3 method for class 'cluster_spec'
extract_parameter_set_dials(x, ...)
```

## Arguments

- x:

  A
  [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
  object or a
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object.

- ...:

  Not currently used.

## Value

The extracted value from the tidyclust object, `x`, as described in the
description section.

## Details

Extracting the underlying engine fit can be helpful for describing the
model (via [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), etc.) or for
variable importance/explainers.

However, users should not invoke the
[`predict()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)
method on an extracted model. There may be preprocessing operations that
`tidyclust` has executed on the data prior to giving it to the model.
Bypassing these can lead to errors or silently generating incorrect
predictions.

**Good**:

       tidyclust_fit |> predict(new_data)

**Bad**:

       tidyclust_fit |> extract_fit_engine() |> predict(new_data)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 2)
kmeans_fit <- fit(kmeans_spec, ~., data = mtcars)

extract_fit_engine(kmeans_fit)
#> K-means clustering with 2 clusters of sizes 18, 14
#> 
#> Cluster means:
#>        mpg      cyl     disp        hp     drat       wt     qsec
#> 1 23.97222 4.777778 135.5389  98.05556 3.882222 2.609056 18.68611
#> 2 15.10000 8.000000 353.1000 209.21429 3.229286 3.999214 16.77214
#>          vs        am     gear     carb
#> 1 0.7777778 0.6111111 4.000000 2.277778
#> 2 0.0000000 0.1428571 3.285714 3.500000
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                   1                   1                   1 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                   1                   2                   1 
#>          Duster 360           Merc 240D            Merc 230 
#>                   2                   1                   1 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   2 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                   2                   2                   2 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                   2                   2                   1 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                   1                   1                   1 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   2                   2                   2 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                   2                   1                   1 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                   1                   2                   1 
#>       Maserati Bora          Volvo 142E 
#>                   2                   1 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 58920.54 93643.90
#>  (between_SS / total_SS =  75.5 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"    
#> [5] "tot.withinss" "betweenss"    "size"         "iter"        
#> [9] "ifault"      
```
