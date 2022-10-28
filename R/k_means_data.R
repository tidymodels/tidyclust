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
      protect = c("x", "centers"),
      func = c(pkg = "stats", fun = "kmeans"),
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
    func = list(pkg = "tidyclust", fun = "num_clusters"),
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
      func = c(fun = "stats_kmeans_predict"),
      args =
        list(
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
      func = c(pkg = "tidyclust", fun = "ClusterR_kmeans_fit"),
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
    func = list(pkg = "tidyclust", fun = "num_clusters"),
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
      func = c(fun = "clusterR_kmeans_predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )
}
