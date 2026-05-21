


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 1 tuning parameters:

- `bandwidth`: Bandwidth (type: double, default: no default)

## Translation from tidyclust to the original package (partition)


``` r
mean_shift(bandwidth = 0.5) |>
  set_engine("meanShiftR") |>
  set_mode("partition") |>
  translate_tidyclust()
```

```
## Mean Shift Clustering Specification (partition)
## 
## Main Arguments:
##   bandwidth = 0.5
## 
## Computational engine: meanShiftR 
## 
## Model fit template:
## tidyclust::.mean_shift_fit_meanShiftR(x = missing_arg(), bandwidth = missing_arg(), 
##     bandwidth = 0.5)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.cluster_spec]{fit()}}, tidyclust will convert factor columns to indicators.

Unlike the `LPCM` engine, `meanShiftR::meanShift()` does not scale variables internally and operates on the raw data scale. The `bandwidth` value is used directly as a per-dimension kernel width on the original variables, and a scalar bandwidth is recycled to a per-column vector. Because of this, appropriate bandwidths typically depend on the spread of the predictors. Standardizing predictors before fitting (for example, with [recipes::step_normalize()]) is recommended; otherwise the default `dials::bandwidth()` range of `c(0.01, 1)` may be too narrow.

## What does it mean to predict?

To predict the cluster assignment for a new observation, the mean shift procedure is run from the new point against the training data's kernel density estimate. The observation is assigned to the cluster whose training mode is closest to the converged value by Euclidean distance.

## References

- Cheng, Y. (1995). Mean shift, mode seeking, and clustering. IEEE Transactions on Pattern Analysis and Machine Intelligence, 17(8), 790–799. https://ieeexplore.ieee.org/document/400568

- Comaniciu, D., & Meer, P. (2002). Mean shift: A robust approach toward feature space analysis. IEEE Transactions on Pattern Analysis and Machine Intelligence, 24(5), 603–619. https://ieeexplore.ieee.org/document/1000236

- Lisic, J. (2015). Parcel Level Agricultural Land Cover Prediction (Doctoral dissertation, George Mason University).
