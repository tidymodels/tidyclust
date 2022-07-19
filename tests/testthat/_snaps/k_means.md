# bad input

    Code
      k_means(mode = "bogus")
    Error <rlang_error>
      'bogus' is not a known mode for model `k_means()`.

---

    Code
      bt <- k_means(k = -1) %>% set_engine("stats")
      fit(bt, mpg ~ ., mtcars)
    Error <rlang_error>
      The number of centers should be >= 0.

---

    Code
      translate_tidyclust(k_means(), engine = NULL)
    Error <rlang_error>
      Please set an engine.

---

    Code
      translate_tidyclust(k_means(formula = ~x))
    Error <simpleError>
      unused argument (formula = ~x)

# printing

    Code
      k_means()
    Output
      K Means Cluster Specification (partition)
      
      Computational engine: stats 
      

---

    Code
      k_means(k = 10)
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        k = 10
      
      Computational engine: stats 
      

# updating

    Code
      k_means(k = 5) %>% update(k = tune())
    Output
      K Means Cluster Specification (partition)
      
      Main Arguments:
        k = tune()
      
      Computational engine: stats 
      

