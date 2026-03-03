# nocov start

make_freq_itemsets <- function() {
  modelenv::set_new_model("freq_itemsets")

  modelenv::set_model_mode("freq_itemsets", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("freq_itemsets", "partition", "arules")
  modelenv::set_dependency(
    model = "freq_itemsets",
    mode = "partition",
    eng = "arules",
    pkg = "arules"
  )
  modelenv::set_dependency(
    model = "freq_itemsets",
    mode = "partition",
    eng = "arules",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "freq_itemsets",
    eng = "arules",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("x"),
      func = c(pkg = "tidyclust", fun = ".freq_itemsets_fit_arules"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "freq_itemsets",
    eng = "arules",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "freq_itemsets",
    eng = "arules",
    exposed = "min_support",
    original = "min_support",
    func = list(pkg = "dials", fun = "min_support"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "freq_itemsets",
    eng = "arules",
    exposed = "mining_method",
    original = "mining_method",
    func = list(pkg = "tidyclust", fun = "mining_method"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "freq_itemsets",
    eng = "arules",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".freq_itemsets_predict_arules"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

  # May want to change to pre and post instead of direct function
  modelenv::set_pred(
    model = "freq_itemsets",
    eng = "arules",
    mode = "partition",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".freq_itemsets_predict_raw_arules"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )
}
