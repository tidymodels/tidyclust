stats_kmeans_predict <- function(object, new_data, prefix = "Cluster_") {
  res <- object$centers[unique(object$cluster), , drop = FALSE]
  res <- flexclust::dist2(res, new_data)
  res <- apply(res, 2, which.min)
  res <- paste0(prefix, res)
  factor(res)
}

clusterR_kmeans_predict <- function(object, new_data, prefix = "Cluster_") {
  clusters <- predict(object, new_data)
  n_clusters <- length(object$obs_per_cluster)

  reorder_clusts <- order(union(unique(clusters), seq_len(n_clusters)))
  names <- paste0(prefix, seq_len(n_clusters))
  res <- names[reorder_clusts][clusters]

  factor(res)
}

stats_hier_clust_predict <- function(object, new_data, ..., prefix = "Cluster_") {
  linkage_method <- object$method

  new_data <- as.matrix(new_data)

  training_data <- as.matrix(attr(object, "training_data"))
  clusters <- extract_cluster_assignment(object, ..., prefix = prefix)

  if (linkage_method %in% c("single", "complete", "average", "median")) {
    ## complete, single, average, and median linkage_methods are basically the
    ## same idea, just different summary distance to cluster

    cluster_dist_fun <- switch(linkage_method,
      "single" = min,
      "complete" = max,
      "average" = mean,
      "median" = stats::median
    )

    # need this to be obs on rows, dist to new data on cols
    dists_new <- Rfast::dista(xnew = training_data, x = new_data, trans = TRUE)

    cluster_dists <- dplyr::bind_cols(data.frame(dists_new), clusters) %>%
      dplyr::group_by(.cluster) %>%
      dplyr::summarize_all(cluster_dist_fun)

    pred_clusts_num <- cluster_dists %>%
      dplyr::select(-.cluster) %>%
      map_dbl(which.min)
  } else if (linkage_method == "centroid") {
    ## Centroid linkage_method, dist to center

    cluster_centers <- extract_centroids(object) %>% dplyr::select(-.cluster)
    dists_means <- Rfast::dista(new_data, cluster_centers)

    pred_clusts_num <- apply(dists_means, 1, which.min)
  } else if (linkage_method %in% c("ward.D", "ward", "ward.D2")) {
    ## Ward linkage_method: lowest change in ESS
    ## dendrograms created from already-squared distances
    ## use Ward.D2 on these plain distances for Ward.D

    cluster_centers <- extract_centroids(object)
    n_clust <- nrow(cluster_centers)
    cluster_names <- cluster_centers[[1]]
    cluster_centers <- as.matrix(cluster_centers[, -1])

    d_means <- map(
      seq_len(n_clust),
      ~ t(
        t(training_data[clusters$.cluster == cluster_names[.x], ]) -
          cluster_centers[.x, ]
      )
    )

    d_new_list <- map(
      seq_len(nrow(new_data)),
      function(new_obs) {
        map(
          seq_len(n_clust),
          ~ t(t(training_data[clusters$.cluster == cluster_names[.x], ])
          - new_data[new_obs, ])
        )
      }
    )

    n <- nrow(training_data)

    change_in_ess <- map(
      d_new_list,
      function(v) {
        map2_dbl(
          d_means, v,
          ~ sum((n * .x + .y)^2 / (n + 1)^2 - .x^2)
        )
      }
    )

    pred_clusts_num <- map_dbl(change_in_ess, which.min)
  } else {
    rlang::abort(
      glue::glue(
        "linkage_method {linkage_method} is not supported for prediction."
      )
    )
  }
  pred_clusts <- unique(clusters$.cluster)[pred_clusts_num]

  return(factor(pred_clusts))
}
