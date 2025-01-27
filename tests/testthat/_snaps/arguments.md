# pipe arguments

    Code
      k_means() %>% set_args()
    Condition
      Error in `set_args()`:
      ! Please pass at least one named argument.

# pipe engine

    Code
      k_means() %>% set_mode()
    Condition
      Error in `modelenv::stop_incompatible_mode()`:
      x Available modes for model type k_means are:
      * "unknown" and "partition".

---

    Code
      k_means() %>% set_mode(2)
    Condition
      Error in `set_mode()`:
      ! 2 is not a known mode for model `k_means()`.

---

    Code
      k_means() %>% set_mode("haberdashery")
    Condition
      Error in `set_mode()`:
      ! "haberdashery" is not a known mode for model `k_means()`.

# can't set a mode that isn't allowed by the model spec

    Code
      set_mode(k_means(), "classification")
    Condition
      Error in `set_mode()`:
      ! "classification" is not a known mode for model `k_means()`.

