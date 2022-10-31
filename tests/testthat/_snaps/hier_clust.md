# bad input

    Code
      hier_clust(mode = "bogus")
    Condition
      Error in `modelenv::check_spec_mode_engine_val()`:
      ! 'bogus' is not a known mode for model `hier_clust()`.

---

    Code
      bt <- hier_clust(linkage_method = "bogus") %>% set_engine("stats")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `stats::hclust()`:
      ! invalid clustering method bogus

---

    Code
      translate_tidyclust(hier_clust(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(hier_clust(formula = ~x))
    Condition
      Error in `hier_clust()`:
      ! unused argument (formula = ~x)

# printing

    Code
      hier_clust()
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        linkage_method = complete
      
      Computational engine: stats 
      

---

    Code
      hier_clust(num_clusters = 10)
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = 10
        linkage_method = complete
      
      Computational engine: stats 
      

# updating

    Code
      hier_clust(num_clusters = 5) %>% update(num_clusters = tune())
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        num_clusters = tune()
        linkage_method = complete
      
      Computational engine: stats 
      

