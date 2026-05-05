.mean_shift_predict_LPCM <- function(
  object,
  new_data,
  prefix = "Cluster_",
  labels = NULL
) {
  modes <- object$cluster.center
  scaled_by <- object$scaled.by
  h <- object$h
  X_scaled <- as.matrix(object$data)

  new_data <- as.matrix(new_data)
  new_data_scaled <- sweep(new_data, 2, scaled_by, "/")

  clusters <- vapply(
    seq_len(nrow(new_data_scaled)),
    function(i) {
      final <- LPCM::ms.rep(X = X_scaled, x = new_data_scaled[i, ], h = h)$final
      dists <- rowSums(sweep(modes, 2, final, "-")^2)
      which.min(dists)
    },
    integer(1)
  )

  n_clusters <- nrow(modes)
  make_predictions(clusters, prefix, n_clusters, labels)
}

#' @export
extract_cluster_assignment.ms <- function(object, ...) {
  n_clusters <- nrow(object$cluster.center)
  cluster_assignment_tibble(object$cluster.label, n_clusters, ...)
}

#' @export
extract_fit_summary.ms <- function(
  object,
  ...,
  prefix = "Cluster_",
  labels = NULL
) {
  n_clusters <- nrow(object$cluster.center)
  names <- make_cluster_labels(n_clusters, prefix, labels)
  names <- factor(names)

  cluster_assignments <- factor(
    names[object$cluster.label],
    levels = levels(names)
  )

  centroids <- sweep(object$cluster.center, 2, object$scaled.by, "*")
  centroids <- tibble::as_tibble(centroids, .name_repair = "minimal")
  if (ncol(centroids) > 0 && all(colnames(centroids) == "")) {
    colnames(centroids) <- colnames(object$data)
  }

  n_members <- as.integer(table(factor(
    object$cluster.label,
    levels = seq_len(n_clusters)
  )))

  list(
    cluster_names = names,
    centroids = centroids,
    n_members = n_members,
    sse_within_total_total = NULL,
    sse_total = NULL,
    orig_labels = object$cluster.label,
    cluster_assignments = cluster_assignments
  )
}
