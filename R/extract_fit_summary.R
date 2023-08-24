#' S3 method to get fitted model summary info depending on engine
#'
#' @param object a fitted [`cluster_spec`] object
#' @param ... other arguments passed to methods
#'
#' @return A list with various summary elements
#'
#' @details
#'
#' The elements `cluster_names` and `cluster_assignments` will be factors.
#'
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) %>%
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_fit_summary()
#' @export
extract_fit_summary <- function(object, ...) {
  UseMethod("extract_fit_summary")
}

#' @export
extract_fit_summary.cluster_spec <- function(object, ...,
                                             call = rlang::caller_env(n = 0)) {
  rlang::abort(
    paste(
      "This function requires a fitted model.",
      "Please use `fit()` on your cluster specification."
    ),
    call = call
  )
}

#' @export
extract_fit_summary.cluster_fit <- function(object, ...) {
  extract_fit_summary(object$fit, ...)
}

#' @export
extract_fit_summary.workflow <- function(object, ...) {
  extract_fit_summary(object$fit$fit$fit, ...)
}

#' @export
extract_fit_summary.kmeans <- function(object, ..., prefix = "Cluster_") {
  reorder_clusts <- order(unique(object$cluster))
  names <- paste0(prefix, seq_len(nrow(object$centers)))
  names <- factor(names)

  cluster_asignments <- factor(
    names[reorder_clusts][object$cluster],
    levels = levels(names)
  )

  centroids <- object$centers[reorder_clusts, , drop = FALSE]
  centroids <- tibble::as_tibble(centroids)

  list(
    cluster_names = names,
    centroids = centroids,
    n_members = object$size[unique(object$cluster)],
    sse_within_total_total = object$withinss[unique(object$cluster)],
    sse_total = object$totss,
    orig_labels = unname(object$cluster),
    cluster_assignments = cluster_asignments
  )
}

#' @export
extract_fit_summary.KMeansCluster <- function(object,
                                              ...,
                                              prefix = "Cluster_") {
  reorder_clusts <- order(unique(object$cluster))
  names <- paste0(prefix, seq_len(nrow(object$centroids)))
  names <- factor(names)

  cluster_asignments <- factor(
    names[reorder_clusts][object$clusters],
    levels = levels(names)
  )

  centroids <- object$centroids[reorder_clusts, , drop = FALSE]
  centroids <- tibble::as_tibble(centroids)

  list(
    cluster_names = names,
    centroids = centroids,
    n_members = as.integer(object$obs_per_cluster[unique(object$cluster)]),
    sse_within_total_total = object$WCSS_per_cluster[unique(object$cluster)],
    sse_total = object$total_SSE,
    orig_labels = object$clusters,
    cluster_assignments = cluster_asignments
  )
}

#' @export
extract_fit_summary.kproto <- function(object,
                                       ...,
                                       prefix = "Cluster_") {
  reorder_clusts <- order(unique(object$cluster))
  names <- paste0(prefix, seq_len(nrow(object$centers)))
  names <- factor(names)

  cluster_asignments <- factor(
    names[reorder_clusts][object$cluster],
    levels = levels(names)
  )

  centroids <- object$centers[reorder_clusts, , drop = FALSE]
  centroids <- tibble::as_tibble(centroids)

  list(
    cluster_names = names,
    centroids = centroids,
    n_members = as.integer(object$size[unique(object$cluster)]),
    sse_within_total_total = object$withinss[unique(object$cluster)],
    sse_total = object$tot.withinss,
    orig_labels = seq_len(length(table(object$cluster))),
    cluster_assignments = cluster_asignments
  )
}

#' @export
extract_fit_summary.hclust <- function(object, ...) {
  clusts <- extract_cluster_assignment(object, ...)$.cluster
  n_clust <- dplyr::n_distinct(clusts)

  training_data <- attr(object, "training_data")

  overall_centroid <- colMeans(training_data)

  by_clust <- training_data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      .cluster = clusts
    ) %>%
    dplyr::group_by(.cluster) %>%
    tidyr::nest()

  centroids <- by_clust$data %>%
    map(dplyr::summarize_all, mean) %>%
    dplyr::bind_rows()

  sse_within_total_total <- map2_dbl(
    by_clust$data,
    seq_len(n_clust),
    ~ sum(Rfast::dista(centroids[.y, ], .x))
  )

  list(
    cluster_names = unique(clusts),
    centroids = centroids,
    n_members = unname(as.integer(table(clusts))),
    sse_within_total_total = sse_within_total_total,
    sse_total = sum(Rfast::dista(t(overall_centroid), training_data)),
    orig_labels = NULL,
    cluster_assignments = clusts
  )
}
