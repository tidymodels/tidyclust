# nocov start

make_hier_clust <- function() {
  modelenv::set_new_model("hier_clust")

  modelenv::set_model_mode("hier_clust", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("hier_clust", "partition", "stats")
  modelenv::set_dependency(
    model = "hier_clust",
    mode = "partition",
    eng = "stats",
    pkg = "stats"
  )
  modelenv::set_dependency(
    model = "hier_clust",
    mode = "partition",
    eng = "stats",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "hier_clust",
    eng = "stats",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("data"),
      func = c(pkg = "tidyclust", fun = ".hier_clust_fit_stats"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "hier_clust",
    eng = "stats",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "hier_clust",
    eng = "stats",
    exposed = "num_clusters",
    original = "num_clusters",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "hier_clust",
    eng = "stats",
    exposed = "linkage_method",
    original = "linkage_method",
    func = list(pkg = "tidyclust", fun = "linkage_method"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "hier_clust",
    eng = "stats",
    exposed = "cut_height",
    original = "cut_height",
    func = list(pkg = "tidyclust", fun = "cut_height"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "hier_clust",
    eng = "stats",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".hier_clust_predict_stats"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

# nocov end
