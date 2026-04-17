# bad input

    Code
      gm_clust(mode = "bogus")
    Condition
      Error in `gm_clust()`:
      ! "bogus" is not a known mode for model `gm_clust()`.

---

    Code
      bt <- gm_clust(circular = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The circular cluster shape argument should be TRUE or FALSE.

---

    Code
      bt <- gm_clust(zero_covariance = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The zero covariance argument should be TRUE or FALSE.

---

    Code
      bt <- gm_clust(shared_orientation = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The shared cluster orientation argument should be TRUE or FALSE.

---

    Code
      bt <- gm_clust(shared_shape = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The shared cluster shape argument should be TRUE or FALSE.

---

    Code
      bt <- gm_clust(shared_size = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The shared cluster size argument should be TRUE or FALSE.

---

    Code
      bt <- gm_clust(num_clusters = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Warning in `sort()`:
      NAs introduced by coercion
      Error in `if (G[1] == 1) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      translate_tidyclust(gm_clust(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(gm_clust(formula = ~x))
    Condition
      Error in `gm_clust()`:
      ! unused argument (formula = ~x)

# printing

    Code
      gm_clust()
    Output
      GMM Clustering Specification (partition)
      
      Main Arguments:
        circular = TRUE
        zero_covariance = TRUE
        shared_orientation = TRUE
        shared_shape = TRUE
        shared_size = TRUE
      
      Computational engine: mclust 
      

---

    Code
      gm_clust(num_clusters = 3)
    Output
      GMM Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = 3
        circular = TRUE
        zero_covariance = TRUE
        shared_orientation = TRUE
        shared_shape = TRUE
        shared_size = TRUE
      
      Computational engine: mclust 
      

# updating

    Code
      gm_clust(num_clusters = 5) %>% update(num_clusters = tune())
    Output
      GMM Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = tune()
        circular = TRUE
        zero_covariance = TRUE
        shared_orientation = TRUE
        shared_shape = TRUE
        shared_size = TRUE
      
      Computational engine: mclust 
      

