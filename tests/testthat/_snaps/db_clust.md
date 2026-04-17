# bad input

    Code
      db_clust(mode = "bogus")
    Condition
      Error in `db_clust()`:
      ! "bogus" is not a known mode for model `db_clust()`.

---

    Code
      bt <- db_clust(radius = -1) %>% set_engine("dbscan")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The radius used to create a cluster should be > 0.

---

    Code
      bt <- db_clust(min_points = -1) %>% set_engine("dbscan")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The number of points in a cluster should be > 0.

---

    Code
      translate_tidyclust(db_clust(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(db_clust(formula = ~x))
    Condition
      Error in `db_clust()`:
      ! unused argument (formula = ~x)

# printing

    Code
      db_clust()
    Output
      DBSCAN Clustering Specification (partition)
      
      Computational engine: dbscan 
      

---

    Code
      db_clust(radius = 20, min_points = 3)
    Output
      DBSCAN Clustering Specification (partition)
      
      Main Arguments:
        radius = 20
        min_points = 3
      
      Computational engine: dbscan 
      

# updating

    Code
      db_clust(radius = 20, min_points = 5) %>% update(radius = tune())
    Output
      DBSCAN Clustering Specification (partition)
      
      Main Arguments:
        radius = tune()
        min_points = 5
      
      Computational engine: dbscan 
      

# errors if `radius` and `min_points` aren't specified

    Code
      db_clust() %>% set_engine("dbscan") %>% fit(~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please specify `radius` to be able to fit specification.

# errors if `radius` isn't specified

    Code
      db_clust(min_points = 10) %>% set_engine("dbscan") %>% fit(~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please specify `radius` to be able to fit specification.

# errors if `min_points` isn't specified

    Code
      db_clust(radius = 20) %>% set_engine("dbscan") %>% fit(~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please specify `min_points` to be able to fit specification.

