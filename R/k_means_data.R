set_new_model_celery("k_means")

set_model_mode_celery("k_means", "partition")

# ------------------------------------------------------------------------------

set_model_engine_celery("k_means", "partition", "stats")
set_dependency_celery("k_means", "stats", "stats")

set_fit_celery(
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

set_encoding_celery(
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

set_model_arg_celery(
  model = "k_means",
  eng = "stats",
  celery = "k",
  original = "centers",
  func = list(pkg = "celery", fun = "k"),
  has_submodel = TRUE
)

set_pred_celery(
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

# ------------------------------------------------------------------------------

set_model_engine_celery("k_means", "partition", "ClusterR")
set_dependency_celery("k_means", "ClusterR", "ClusterR")

set_fit_celery(
  model = "k_means",
  eng = "ClusterR",
  mode = "partition",
  value = list(
    interface = "matrix",
    data = c(x = "data"),
    protect = c("data", "clusters"),
    func = c(pkg = "celery", fun = "ClusterR_kmeans_fit"),
    defaults = list()
  )
)

set_encoding_celery(
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

set_model_arg_celery(
  model = "k_means",
  eng = "ClusterR",
  celery = "k",
  original = "clusters",
  func = list(pkg = "celery", fun = "k"),
  has_submodel = TRUE
)

set_pred_celery(
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