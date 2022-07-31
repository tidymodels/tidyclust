# pipe arguments

    Code
      k_means() %>% set_args()
    Error <rlang_error>
      Please pass at least one named argument.

# pipe engine

    Code
      k_means() %>% set_mode()
    Error <rlang_error>
      Available modes for model type k_means are: 'unknown', 'partition'

---

    Code
      k_means() %>% set_mode(2)
    Error <rlang_error>
      '2' is not a known mode for model `k_means()`.

---

    Code
      k_means() %>% set_mode("haberdashery")
    Error <rlang_error>
      'haberdashery' is not a known mode for model `k_means()`.

# can't set a mode that isn't allowed by the model spec

    Code
      set_mode(k_means(), "classification")
    Error <rlang_error>
      'classification' is not a known mode for model `k_means()`.

