# nocov start

make_k_means <- function() {
  modelenv::set_new_model("k_means")

  modelenv::set_model_mode("k_means", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("k_means", "partition", "stats")
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "stats",
    pkg = "stats"
  )
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "stats",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "k_means",
    eng = "stats",
    mode = "partition",
    value = list(
      interface = "matrix",
      data = c(x = "data"),
      protect = c("x", "centers"),
      func = c(pkg = "tidyclust", fun = ".k_means_fit_stats"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "k_means",
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
    model = "k_means",
    eng = "stats",
    exposed = "num_clusters",
    original = "centers",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "k_means",
    eng = "stats",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".k_means_predict_stats"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("k_means", "partition", "ClusterR")
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "ClusterR",
    pkg = "ClusterR"
  )
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "ClusterR",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "k_means",
    eng = "ClusterR",
    mode = "partition",
    value = list(
      interface = "matrix",
      data = c(x = "data"),
      protect = c("data", "clusters"),
      func = c(pkg = "tidyclust", fun = ".k_means_fit_ClusterR"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "k_means",
    eng = "ClusterR",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "k_means",
    eng = "ClusterR",
    exposed = "num_clusters",
    original = "clusters",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "k_means",
    eng = "ClusterR",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".k_means_predict_ClusterR"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("k_means", "partition", "clustMixType")
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "clustMixType",
    pkg = "clustMixType"
  )
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "clustMixType",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "k_means",
    eng = "clustMixType",
    mode = "partition",
    value = list(
      interface = "data.frame",
      data = c(x = "x"),
      protect = c("x", "k", "keep.data"),
      func = c(pkg = "tidyclust", fun = ".k_means_fit_clustMixType"),
      defaults = list(keep.data = TRUE, verbose = FALSE)
    )
  )

  modelenv::set_encoding(
    model = "k_means",
    eng = "clustMixType",
    mode = "partition",
    options = list(
      predictor_indicators = "none",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "k_means",
    eng = "clustMixType",
    exposed = "num_clusters",
    original = "k",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "k_means",
    eng = "clustMixType",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".k_means_predict_clustMixType"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("k_means", "partition", "klaR")
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "klaR",
    pkg = "klaR"
  )
  modelenv::set_dependency(
    model = "k_means",
    mode = "partition",
    eng = "klaR",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "k_means",
    eng = "klaR",
    mode = "partition",
    value = list(
      interface = "data.frame",
      data = c(x = "data"),
      protect = c("data", "modes"),
      func = c(pkg = "tidyclust", fun = ".k_means_fit_klaR"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "k_means",
    eng = "klaR",
    mode = "partition",
    options = list(
      predictor_indicators = "none",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "k_means",
    eng = "klaR",
    exposed = "num_clusters",
    original = "modes",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "k_means",
    eng = "klaR",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".k_means_predict_klaR"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )
}

# nocov end
