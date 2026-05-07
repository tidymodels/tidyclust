# nocov start

make_mean_shift <- function() {
  modelenv::set_new_model("mean_shift")

  modelenv::set_model_mode("mean_shift", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("mean_shift", "partition", "LPCM")
  modelenv::set_dependency(
    model = "mean_shift",
    mode = "partition",
    eng = "LPCM",
    pkg = "LPCM"
  )
  modelenv::set_dependency(
    model = "mean_shift",
    mode = "partition",
    eng = "LPCM",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "mean_shift",
    eng = "LPCM",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("x", "bandwidth"),
      func = c(pkg = "tidyclust", fun = ".mean_shift_fit_LPCM"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "mean_shift",
    eng = "LPCM",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "mean_shift",
    eng = "LPCM",
    exposed = "bandwidth",
    original = "bandwidth",
    func = list(pkg = "tidyclust", fun = "bandwidth"),
    has_submodel = FALSE
  )

  modelenv::set_pred(
    model = "mean_shift",
    eng = "LPCM",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".mean_shift_predict_LPCM"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

# nocov end
