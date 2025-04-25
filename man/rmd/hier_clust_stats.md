


For this engine, there is a single mode: partition

## Tuning Parameters



This model has 1 tuning parameters:

- `num_clusters`: # Clusters (type: integer, default: no default)

## Translation from tidyclust to the original package (partition)


```r
hier_clust(num_clusters = integer(1)) |> 
  set_engine("stats") |> 
  set_mode("partition") |> 
  translate_tidyclust()
```

```
## Hierarchical Clustering Specification (partition)
## 
## Main Arguments:
##   num_clusters = integer(1)
##   linkage_method = complete
## 
## Computational engine: stats 
## 
## Model fit template:
## tidyclust::.hier_clust_fit_stats(data = missing_arg(), num_clusters = integer(1), 
##     linkage_method = "complete")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.cluster_spec]{fit()}}, tidyclust will convert factor columns to indicators.

## References

- Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988). The New S Language. Wadsworth & Brooks/Cole. (S version.)

- Everitt, B. (1974). Cluster Analysis. London: Heinemann Educ. Books.

- Hartigan, J.A. (1975). Clustering Algorithms. New York: Wiley.

- Sneath, P. H. A. and R. R. Sokal (1973). Numerical Taxonomy. San Francisco: Freeman.

- Anderberg, M. R. (1973). Cluster Analysis for Applications. Academic Press: New York.

- Gordon, A. D. (1999). Classification. Second Edition. London: Chapman and Hall / CRC

- Murtagh, F. (1985). “Multidimensional Clustering Algorithms”, in COMPSTAT Lectures 4. Wuerzburg: Physica-Verlag (for algorithmic details of algorithms used).

- McQuitty, L.L. (1966). Similarity Analysis by Reciprocal Pairs for Discrete and Continuous Data. Educational and Psychological Measurement, 26, 825–831. doi:10.1177/001316446602600402.

- Legendre, P. and L. Legendre (2012). Numerical Ecology, 3rd English ed. Amsterdam: Elsevier Science BV.

- Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification, 31, 274–295. doi:10.1007/s00357-014-9161-z.
