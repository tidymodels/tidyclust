


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 1 tuning parameters:

- `num_clusters`: # Clusters (type: integer, default: no default)

## Translation from tidyclust to the original package (partition)


```r
k_means(num_clusters = integer(1)) |> 
  set_engine("ClusterR") |> 
  set_mode("partition") |> 
  translate_tidyclust()
```

```
## K Means Cluster Specification (partition)
## 
## Main Arguments:
##   num_clusters = integer(1)
## 
## Computational engine: ClusterR 
## 
## Model fit template:
## tidyclust::.k_means_fit_ClusterR(data = missing_arg(), clusters = missing_arg(), 
##     clusters = integer(1))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.cluster_spec]{fit()}}, tidyclust will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## References

- Forgy, E. W. (1965). Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics, 21, 768–769.

- Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108. doi:10.2307/2346830.

- Lloyd, S. P. (1957, 1982). Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory, 28, 128–137.

- MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press.
