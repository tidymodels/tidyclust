set_new_model("k_means")

set_model_mode("k_means", "partition")

# ------------------------------------------------------------------------------

set_model_engine("k_means", "partition", "stats")
set_dependency("k_means", "stats", "stats")

set_fit(
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

set_encoding(
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

set_model_arg(
  model = "k_means",
  eng = "stats",
  celery = "k",
  original = "centers",
  func = list(pkg = "dials", fun = "k"),
  has_submodel = TRUE
)

#
# set_pred(
#   model = "k_means",
#   eng = "stats",
#   mode = "partition",
#   type = "numeric",
#   value = list(
#     pre = NULL,
#     post = NULL,
#     func = c(fun = "predict"),
#     args =
#       list(
#         object = expr(object$fit),
#         newdata = expr(new_data),
#         type = "response"
#       )
#   )
# )
#
# set_pred(
#   model = "k_means",
#   eng = "stats",
#   mode = "partition",
#   type = "conf_int",
#   value = list(
#     pre = NULL,
#     post = function(results, object) {
#       tibble::as_tibble(results) %>%
#         dplyr::select(-fit) %>%
#         setNames(c(".pred_lower", ".pred_upper"))
#     },
#     func = c(fun = "predict"),
#     args =
#       list(
#         object = expr(object$fit),
#         newdata = expr(new_data),
#         interval = "confidence",
#         level = expr(level),
#         type = "response"
#       )
#   )
# )
# set_pred(
#   model = "k_means",
#   eng = "stats",
#   mode = "partition",
#   type = "pred_int",
#   value = list(
#     pre = NULL,
#     post = function(results, object) {
#       tibble::as_tibble(results) %>%
#         dplyr::select(-fit) %>%
#         setNames(c(".pred_lower", ".pred_upper"))
#     },
#     func = c(fun = "predict"),
#     args =
#       list(
#         object = expr(object$fit),
#         newdata = expr(new_data),
#         interval = "prediction",
#         level = expr(level),
#         type = "response"
#       )
#   )
# )
#
# set_pred(
#   model = "k_means",
#   eng = "stats",
#   mode = "partition",
#   type = "raw",
#   value = list(
#     pre = NULL,
#     post = NULL,
#     func = c(fun = "predict"),
#     args = list(object = expr(object$fit), newdata = expr(new_data))
#   )
# )
