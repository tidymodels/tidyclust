


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 1 tuning parameters:

- `bandwidth`: Bandwidth (type: double, default: no default)

## Translation from tidyclust to the original package (partition)


``` r
mean_shift(bandwidth = 0.5) %>%
  set_engine("LPCM") %>%
  set_mode("partition") %>%
  translate_tidyclust()
```

```
## Mean Shift Clustering Specification (partition)
## 
## Main Arguments:
##   bandwidth = 0.5
## 
## Computational engine: LPCM 
## 
## Model fit template:
## tidyclust::.mean_shift_fit_LPCM(x = missing_arg(), bandwidth = missing_arg(), 
##     bandwidth = 0.5)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.cluster_spec]{fit()}}, tidyclust will convert factor columns to indicators.

`LPCM::ms()` scales each variable internally to the unit range before applying
the Gaussian kernel, so the `bandwidth` value lives on the scaled scale rather
than the raw data scale. Bandwidths between roughly `0.05` and `1` are typical;
smaller values find more clusters and larger values merge them.

## What does it mean to predict?

To predict the cluster assignment for a new observation, the mean shift
procedure is run from the new point until it converges to a mode. The
observation is then assigned to the cluster of the nearest discovered training
mode by Euclidean distance.


## References

- Cheng, Y. (1995). Mean shift, mode seeking, and clustering. IEEE Transactions on Pattern Analysis and Machine Intelligence, 17(8), 790–799. https://doi.org/10.1109/34.400568

- Comaniciu, D., & Meer, P. (2002). Mean shift: A robust approach toward feature space analysis. IEEE Transactions on Pattern Analysis and Machine Intelligence, 24(5), 603–619. https://doi.org/10.1109/34.1000236

- Einbeck, J., Evers, L., & Hinchliff, K. (2010). Data compression and regression based on local principal curves. In A. Fink, B. Lausen, W. Seidel, & A. Ultsch (Eds.), Advances in Data Analysis, Data Handling and Business Intelligence (pp. 701–712). Springer.
