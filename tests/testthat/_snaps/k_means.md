# bad input

    Code
      k_means(mode = "bogus")
    Condition
      Error in `k_means()`:
      ! "bogus" is not a known mode for model `k_means()`.

---

    Code
      bt <- set_engine(k_means(num_clusters = -1), "stats")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The number of centers should be >= 0.

---

    Code
      translate_tidyclust(k_means(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(k_means(formula = ~x))
    Condition
      Error in `k_means()`:
      ! unused argument (formula = ~x)

# printing

    Code
      k_means()
    Output
      K Means Cluster Specification (partition)
      
      Computational engine: stats 
      

---

    Code
      k_means(num_clusters = 10)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = 10
      
      Computational engine: stats 
      

# updating

    Code
      update(k_means(num_clusters = 5), num_clusters = tune())
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        num_clusters = tune()
      
      Computational engine: stats 
      

# errors if `num_clust` isn't specified

    Code
      fit(set_engine(k_means(), "stats"), ~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please specify `num_clust` to be able to fit specification.

---

    Code
      fit(set_engine(k_means(), "ClusterR"), ~., data = mtcars)
    Condition
      Error in `tidyclust::.k_means_fit_ClusterR()`:
      ! argument "clusters" is missing, with no default

