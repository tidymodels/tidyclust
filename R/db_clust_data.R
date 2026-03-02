# nocov start

make_db_clust <- function() {
  modelenv::set_new_model("db_clust")

  modelenv::set_model_mode("db_clust", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("db_clust", "partition", "dbscan")
  modelenv::set_dependency(
    model = "db_clust",
    mode = "partition",
    eng = "dbscan",
    pkg = "dbscan"
  )
  modelenv::set_dependency(
    model = "db_clust",
    mode = "partition",
    eng = "dbscan",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "db_clust",
    eng = "dbscan",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("x", "radius", "min_points"),
      func = c(pkg = "tidyclust", fun = ".db_clust_fit_dbscan"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "db_clust",
    eng = "dbscan",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "db_clust",
    eng = "dbscan",
    exposed = "radius",
    original = "radius",
    func = list(pkg = "dials", fun = "radius"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "db_clust",
    eng = "dbscan",
    exposed = "min_points",
    original = "min_points",
    func = list(pkg = "dials", fun = "min_points"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "db_clust",
    eng = "dbscan",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".db_clust_predict_dbscan"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

}

# nocov end
