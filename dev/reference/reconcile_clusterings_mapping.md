# Relabels clusters to match another cluster assignment

When forcing one-to-one, the user needs to decide what to prioritize:

- "accuracy": optimize raw count of all observations with the same label
  across the two assignments

- "precision": optimize the average percent of each alt cluster that
  matches the corresponding primary cluster

## Usage

``` r
reconcile_clusterings_mapping(
  primary,
  alternative,
  one_to_one = TRUE,
  optimize = "accuracy"
)
```

## Arguments

- primary:

  A vector containing cluster labels, to be matched

- alternative:

  Another vector containing cluster labels, to be changed

- one_to_one:

  Boolean; should each alt cluster match only one primary cluster?

- optimize:

  One of "accuracy" or "precision"; see description.

## Value

A tibble with 3 columns; `primary`, `alt`, `alt_recoded`

## Details

Retains the cluster labels of the primary assignment, and relabel the
alternate assignment to match as closely as possible. The user must
decide whether clusters are forced to be "one-to-one"; that is, are we
allowed to assign multiple labels from the alternate assignment to the
same primary label?

Cluster labels are arbitrary — two clusterings of the same data may
agree on the groups but use different label names (e.g. "Dog" vs "Apple"
for the same cluster). `reconcile_clusterings_mapping()` is useful when
you want to compare two clusterings, for example:

- Comparing cluster assignments across cross-validation folds.

- Checking stability of a clustering algorithm across different random
  seeds.

- Aligning predicted clusters on new data with the original training
  labels.

## Examples

``` r
factor1 <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
factor2 <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Fish")
reconcile_clusterings_mapping(factor1, factor2)
#> # A tibble: 6 × 3
#>   primary alt   alt_recoded
#>   <chr>   <chr> <chr>      
#> 1 Apple   Dog   Carrot     
#> 2 Apple   Dog   Carrot     
#> 3 Carrot  Cat   Banana     
#> 4 Carrot  Dog   Carrot     
#> 5 Banana  Fish  Apple      
#> 6 Banana  Fish  Apple      

factor1 <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
factor2 <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Parrot")
reconcile_clusterings_mapping(factor1, factor2, one_to_one = FALSE)
#> # A tibble: 6 × 3
#>   primary alt    alt_recoded
#>   <chr>   <chr>  <chr>      
#> 1 Apple   Dog    Apple      
#> 2 Apple   Dog    Apple      
#> 3 Carrot  Cat    Carrot     
#> 4 Carrot  Dog    Apple      
#> 5 Banana  Fish   Banana     
#> 6 Banana  Parrot Banana     
```
