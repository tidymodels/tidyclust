# bad input

    Code
      mean_shift(mode = "bogus")
    Condition
      Error in `mean_shift()`:
      ! "bogus" is not a known mode for model `mean_shift()`.

---

    Code
      bt <- set_engine(mean_shift(bandwidth = -1), "LPCM")
      fit(bt, mpg ~ ., mtcars)
    Condition
      Error in `check_args()`:
      ! The bandwidth used for clustering should be > 0.

---

    Code
      translate_tidyclust(mean_shift(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(mean_shift(formula = ~x))
    Condition
      Error in `mean_shift()`:
      ! unused argument (formula = ~x)

# printing

    Code
      mean_shift()
    Output
      Mean Shift Clustering Specification (partition)
      
      Computational engine: LPCM 
      

---

    Code
      mean_shift(bandwidth = 0.5)
    Output
      Mean Shift Clustering Specification (partition)
      
      Main Arguments:
        bandwidth = 0.5
      
      Computational engine: LPCM 
      

# updating

    Code
      update(mean_shift(bandwidth = 0.5), bandwidth = tune())
    Output
      Mean Shift Clustering Specification (partition)
      
      Main Arguments:
        bandwidth = tune()
      
      Computational engine: LPCM 
      

# errors if `bandwidth` isn't specified

    Code
      fit(set_engine(mean_shift(), "LPCM"), ~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please specify `bandwidth` to be able to fit specification.

