# bad input

    Code
      freq_itemsets(mode = "bogus")
    Condition
      Error in `freq_itemsets()`:
      ! "bogus" is not a known mode for model `freq_itemsets()`.

---

    Code
      bt <- freq_itemsets(min_support = 0.05, mining_method = "bogus")
      fit(bt, ~., toy_df)
    Condition
      Error in `check_args()`:
      ! The mining method should be either 'apriori' or 'eclat'.

---

    Code
      bt <- freq_itemsets(min_support = -1, mining_method = "eclat") %>% set_engine(
        "arules")
      fit(bt, ~., toy_df)
    Condition
      Error in `check_args()`:
      ! The minimum support should be between 0 and 1.

---

    Code
      translate_tidyclust(freq_itemsets(), engine = NULL)
    Condition
      Error in `translate_tidyclust.default()`:
      ! Please set an engine.

---

    Code
      translate_tidyclust(freq_itemsets(formula = ~x))
    Condition
      Error in `freq_itemsets()`:
      ! unused argument (formula = ~x)

# extract_centroids work

    Code
      extract_centroids(fi_fit)
    Condition
      Error in `extract_fit_summary()`:
      ! Centroids are not usfeul for frequent itemsets, we suggust looking at the frequent itemsets directly. Please use arules::inspect() on the fit of your cluster specification.

# printing

    Code
      freq_itemsets()
    Output
      Frequent Itemsets Mining Specification (partition)
      
      Main Arguments:
        mining_method = eclat
      
      Computational engine: arules 
      

---

    Code
      freq_itemsets(min_support = 0.5)
    Output
      Frequent Itemsets Mining Specification (partition)
      
      Main Arguments:
        min_support = 0.5
        mining_method = eclat
      
      Computational engine: arules 
      

# updating

    Code
      freq_itemsets(min_support = 0.5) %>% update(min_support = tune())
    Output
      Frequent Itemsets Mining Specification (partition)
      
      Main Arguments:
        min_support = tune()
        mining_method = eclat
      
      Computational engine: arules 
      

# errors if `min_support` isn't specified

    Code
      freq_itemsets() %>% set_engine("arules") %>% fit(~., data = toy_df)
    Condition
      Error in `tidyclust::.freq_itemsets_fit_arules()`:
      ! Please specify `min_support` to be able to fit specification.

