# nocov start

make_gm_clust <- function() {
  modelenv::set_new_model("gm_clust")

  modelenv::set_model_mode("gm_clust", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("gm_clust", "partition", "mclust")
  modelenv::set_dependency(
    model = "gm_clust",
    mode = "partition",
    eng = "mclust",
    pkg = "mclust"
  )
  modelenv::set_dependency(
    model = "gm_clust",
    mode = "partition",
    eng = "mclust",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("x", "num_clusters", "circular", "zero_covariance", "shared_orientation", "shared_shape", "shared_size"),
      func = c(pkg = "tidyclust", fun = ".gm_clust_fit_mclust"),
      defaults = list()
    )
  )


  modelenv::set_encoding(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "num_clusters",
    original = "num_clusters",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "circular",
    original = "circular",
    func = list(pkg = "dials", fun = "circular"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "zero_covariance",
    original = "zero_covariance",
    func = list(pkg = "dials", fun = "zero_covariance"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "shared_orientation",
    original = "shared_orientation",
    func = list(pkg = "dials", fun = "shared_orientation"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "shared_shape",
    original = "shared_shape",
    func = list(pkg = "dials", fun = "shared_shape"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "shared_size",
    original = "shared_size",
    func = list(pkg = "dials", fun = "shared_size"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".gm_clust_predict_mclust"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

}

# nocov end
