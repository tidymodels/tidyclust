


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 2 tuning parameters:

- `radius`: Radius (type: double, default: no default)

- `min_points`: Minimum Number of Points (type: integer, default: no_default)

## Translation from tidyclust to the original package (partition)


``` r
db_clust(radius = 0.5, min_points = 5)%>% 
  set_engine("dbscan") %>% 
  set_mode("partition") %>% 
  translate_tidyclust()
```

```
## DBSCAN Clustering Specification (partition)
## 
## Main Arguments:
##   radius = 0.5
##   min_points = 5
## 
## Computational engine: dbscan 
## 
## Model fit template:
## tidyclust::.db_clust_fit_dbscan(x = missing_arg(), radius = missing_arg(), 
##     min_points = missing_arg(), radius = 0.5, min_points = 5)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.cluster_spec]{fit()}}, tidyclust will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.


## References

- Ester, M., Kriegel, H.-P., Sander, J., & Xu, X. (1996). A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise.

- Hahsler, M., Piekenbrock, M., & Doran, D. (2019a). Dbscan : Fast Density-Based Clustering with r. Journal of Statistical Software, 91(1). https://www.jstatsoft.org/article/view/v091i01

- Kriegel, H., Kröger, P., Sander, J., & Zimek, A. (2011). Density-based clustering. WIREs Data Mining and Knowledge Discovery, 1(3), 231–240. https://wires.onlinelibrary.wiley.com/doi/10.1002/widm.30. 30

- Tran, T. N., Drab, K., & Daszykowski, M. (2013). Revised DBSCAN algorithm to cluster data with dense adjacent clusters. Chemometrics and Intelligent Laboratory Systems, 49 120, 92–96. https://www.sciencedirect.com/science/article/pii/S0169743912002249


