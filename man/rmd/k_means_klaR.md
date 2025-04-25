


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 1 tuning parameters:

- `num_clusters`: # Clusters (type: integer, default: no default)

## Translation from tidyclust to the original package (partition)


```r
k_means(num_clusters = integer(1)) |> 
  set_engine("klaR") |> 
  set_mode("partition") |> 
  translate_tidyclust()
```

```
## K Means Cluster Specification (partition)
## 
## Main Arguments:
##   num_clusters = integer(1)
## 
## Computational engine: klaR 
## 
## Model fit template:
## tidyclust::.k_means_fit_klaR(data = missing_arg(), modes = missing_arg(), 
##     modes = integer(1))
```

## Preprocessing requirements

Only categorical variables are accepted, along with numerics with few unique values.

## References

- Huang, Z. (1997) A Fast Clustering Algorithm to Cluster Very Large Categorical Data Sets in Data Mining. in KDD: Techniques and Applications (H. Lu, H. Motoda and H. Luu, Eds.), pp. 21-34, World Scientific, Singapore.

- MacQueen, J. (1967) Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281-297. Berkeley, CA: University of California Press.
