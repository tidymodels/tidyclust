# bad input

    Code
      hier_clust(mode = "bogus")
    Error <rlang_error>
      'bogus' is not a known mode for model `hier_clust()`.

---

    Code
      bt <- hier_clust(method = "bogus") %>% set_engine_tidyclust("stats")
    Error <simpleError>
      unused argument (method = "bogus")
    Code
      fit(bt, mpg ~ ., mtcars)
    Error <simpleError>
      object 'bt' not found

---

    Code
      translate_tidyclust(hier_clust(), engine = NULL)
    Error <rlang_error>
      Please set an engine.

---

    Code
      translate_tidyclust(hier_clust(formula = ~x))
    Error <simpleError>
      unused argument (formula = ~x)

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
      hier_clust(k = 10)
    Output
      Hierarchical Clustering Specification (partition)
      
      Main Arguments:
        k = 10
        linkage_method = complete
      
      Computational engine: stats 
      

