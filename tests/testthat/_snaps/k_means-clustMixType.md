# modifies errors about suggested other models

    Code
      fit(set_engine(k_means(num_clusters = 3), "clustMixType"), ~., data = mtcars)
    Condition
      Error in `fit()`:
      ! Engine `clustMixType` requires both numeric and categorical predictors.
      x Only numeric predictors where used.
      i Try using the `stats` engine with `mod |> set_engine("stats")`.

---

    Code
      fit(set_engine(k_means(num_clusters = 3), "clustMixType"), ~., data = data.frame(
        letters, LETTERS))
    Condition
      Error in `fit()`:
      ! Engine `clustMixType` requires both numeric and categorical predictors.
      x Only categorical predictors where used.
      i Try using the `klaR` engine with `mod |> set_engine("klaR")`.

