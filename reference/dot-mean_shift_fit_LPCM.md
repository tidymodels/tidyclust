# Simple Wrapper around LPCM::ms function

This wrapper passes the data and bandwidth to
[`LPCM::ms()`](https://rdrr.io/pkg/LPCM/man/ms.html) with plotting
disabled.

## Usage

``` r
.mean_shift_fit_LPCM(x, bandwidth = NULL, ...)
```

## Arguments

- x:

  matrix or data frame.

- bandwidth:

  Kernel bandwidth controlling the neighborhood size.

## Value

ms object
