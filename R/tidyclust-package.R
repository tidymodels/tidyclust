#' tidyclust: A Tidy Interface to Clustering
#'
#' @description
#' The tidyclust package provides a tidy, unified interface to clustering
#' models, following the same design patterns as parsnip. It creates a
#' consistent API across different clustering functions and engines.
#'
#' @details
#' ## Model specifications
#' - [k_means()]: K-means clustering (stats, ClusterR, klaR, clustMixType
#'   engines)
#' - [hier_clust()]: Hierarchical/agglomerative clustering (stats engine)
#' - [db_clust()]: Density-based clustering (dbscan engine)
#' - [gm_clust()]: Gaussian mixture model clustering (mclust engine)
#'
#' ## Key functions
#' - **Fitting**: [fit()], [fit_xy()]
#' - **Prediction**: [predict.cluster_fit()]
#' - **Extraction**: [extract_centroids()], [extract_cluster_assignment()]
#' - **Metrics**: [silhouette_avg()], [sse_within_total()], [sse_ratio()]
#' - **Tuning**: [tune_cluster()]
#'
#' ## Getting started
#' ```r
#' # Create a specification
#' spec <- k_means(num_clusters = 3)
#'
#' # Fit to data
#' fit <- fit(spec, ~., data = mtcars)
#'
#' # Extract results
#' extract_centroids(fit)
#' extract_cluster_assignment(fit)
#' ```
#'
#' @seealso
#' - Package website: <https://tidyclust.tidymodels.org/>
#' - Bug reports: <https://github.com/tidymodels/tidyclust/issues>
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_cols
#' @importFrom generics tunable tune_args
#' @importFrom mclust mclustBIC
#' @importFrom parsnip make_call
#' @importFrom parsnip maybe_data_frame
#' @importFrom parsnip maybe_matrix
#' @importFrom parsnip model_printer
#' @importFrom parsnip null_value
#' @importFrom parsnip show_call
#' @importFrom rlang %||%
#' @importFrom rlang as_function
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang get_expr
#' @importFrom rlang global_env
#' @importFrom rlang is_logical
#' @importFrom rlang is_true
#' @importFrom rlang missing_arg
#' @importFrom rlang quos
#' @importFrom rlang set_names
#' @importFrom rlang sym
#' @importFrom stats .getXlevels
#' @importFrom stats as.formula
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.offset
#' @importFrom stats model.weights
#' @importFrom stats na.omit
#' @importFrom stats predict
#' @importFrom tibble as_tibble
#' @importFrom utils capture.output
## usethis namespace: end
NULL
