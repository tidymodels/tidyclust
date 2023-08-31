# modifies errors about suggested other models

    Code
      k_means(num_clusters = 3) %>% set_engine("clustMixType") %>% fit(~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Engine `clustMixType` requires both numeric and categorical predictors.
      x Only numeric predictors where used.
      i Try using the `stats` engine with `mod %>% set_engine("stats")`.

---

    Code
      k_means(num_clusters = 3) %>% set_engine("clustMixType") %>% fit(~., data = data.frame(
        letters, LETTERS))
    Condition
      Error in `fit()`:
      ! Engine `clustMixType` requires both numeric and categorical predictors.
      x Only categorical predictors where used.
      i Try using the `klaR` engine with `mod %>% set_engine("klaR")`.

