# Simple Wrapper around meanShiftR::meanShift function

This wrapper passes the data and bandwidth to
[`meanShiftR::meanShift()`](https://rdrr.io/pkg/meanShiftR/man/meanShift.html)
and stashes the training data and bandwidth on the result so they can be
reused for prediction and extraction.

## Usage

``` r
.mean_shift_fit_meanShiftR(x, bandwidth = NULL, ...)
```

## Arguments

- x:

  matrix or data frame.

- bandwidth:

  Kernel bandwidth controlling the neighborhood size. A scalar is
  recycled to a per-column vector.

## Value

A list with class `"ms_meanShiftR"`.
