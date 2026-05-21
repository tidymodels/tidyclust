# One-hot contrast matrix

A re-export of
[`hardhat::contr_one_hot()`](https://hardhat.tidymodels.org/reference/contr_one_hot.html)
for use with `indicators = "one_hot"`.

## Usage

``` r
contr_one_hot(n, contrasts = TRUE, sparse = FALSE)
```

## Arguments

- n:

  A vector of character factor levels (of length \>=1) or the number of
  unique levels (\>= 1).

- contrasts:

  This argument is for backwards compatibility and only the default of
  `TRUE` is supported.

- sparse:

  This argument is for backwards compatibility and only the default of
  `FALSE` is supported.
