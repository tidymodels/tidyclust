#' Extract clusters from model
#'
#' When applied to a fitted cluster specification, returns a tibble with cluster
#' location. When such locations doesn't make sense for the model, a mean
#' location is used.
#'
#' @param object An fitted [`cluster_spec`] object.
#' @param ... Other arguments passed to methods. Using the `prefix` allows you
#'   to change the prefix in the levels of the factor levels.
#'
#' @details
#'
#' Some model types such as K-means as seen in [k_means()] stores the centroid
#' in the object itself. leading the use of this function to act as an simple
#' extract. Other model types such as Hierarchical (Agglomerative) Clustering as
#' seen in [hier_clust()], are fit in such a way that the number of clusters can
#' be determined at any time after the fit. Setting the `num_clusters` or
#' `cut_height` in this function will be used to determine the clustering when
#' reported.
#'
#' Further more, some models like [hier_clust()], doesn't have a notion of
#' "centroids". The mean of the observation within each cluster assignment is
#' returned as the centroid.
#'
#' The ordering of the clusters is such that the first observation in the
#' training data set will be in cluster 1, the next observation that doesn't
#' belong to cluster 1 will be in cluster 2, and so on and forth. As the
#' ordering of clustering doesn't matter, this is done to avoid identical sets
#' of clustering having different labels if fit multiple times.
#'
#' ## Related functions
#'
#' `extract_centroids()` is a part of a trio of functions doing similar things:
#'
#' - [extract_cluster_assignment()] returns the cluster assignments of the
#'   training observations
#' - [extract_centroids()] returns the location of the centroids
#' - \code{\link[=predict.cluster_fit]{predict()}} returns the cluster a new
#'   observation belongs to
#'
#' @return A `tibble::tibble()` with 1 row for each centroid and their position.
#'   `.cluster` denotes the cluster name for the centroid. The remaining
#'   variables match variables passed into model.
#'
#' @seealso [extract_cluster_assignment()] [predict.cluster_fit()]
#'
#' @examples
#' set.seed(1234)
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit |>
#'   extract_centroids()
#'
#' # Some models such as `hier_clust()` fits in such a way that you can specify
#' # the number of clusters after the model is fit.
#' # A Hierarchical (Agglomerative) Clustering method doesn't technically have
#' # clusters, so the center of the observation within each cluster is returned
#' # instead.
#' hclust_spec <- hier_clust() |>
#'   set_engine("stats")
#'
#' hclust_fit <- fit(hclust_spec, ~., mtcars)
#'
#' hclust_fit |>
#'   extract_centroids(num_clusters = 2)
#'
#' hclust_fit |>
#'   extract_centroids(cut_height = 250)
#' @export
extract_centroids <- function(object, ...) {
  summ <- extract_fit_summary(object, ..., call = rlang::caller_env(0))
  clusters <- tibble::tibble(.cluster = summ$cluster_names)
  bind_cols(clusters, summ$centroids)
}
