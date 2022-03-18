# bad input

    Code
      k_means(mode = "bogus")
    Error <rlang_error>
      'bogus' is not a known mode for model `k_means()`.

---

    Code
      bt <- k_means(k = -1) %>% set_engine_celery("stats")
      fit(bt, mpg ~ ., mtcars)
    Error <simpleError>
      invalid 'size' argument

---

    Code
      translate_celery(k_means(), engine = NULL)
    Error <rlang_error>
      Please set an engine.

---

    Code
      translate_celery(k_means(formula = ~x))
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
      

