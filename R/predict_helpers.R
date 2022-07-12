stats_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$cluster)
  res <- apply(flexclust::dist2(object$centers[reorder_clusts, , drop = FALSE], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}

clusterR_kmeans_predict <- function(object, new_data) {
  reorder_clusts <- unique(object$clusters)
  res <- apply(flexclust::dist2(object$centroids[reorder_clusts, , drop = FALSE], new_data), 2, which.min)
  res <- paste0("Cluster_", res)
  factor(res)
}

stats_hier_clust_predict <- function(object, new_data) {

  linkage_method <- object$fit$method

  new_data <- as.matrix(new_data)

  training_data <- as.matrix(attr(object$fit, "training_data"))
  clusters <- extract_cluster_assignment(object)

  if (linkage_method %in% c("single", "complete", "average", "median")) {

    ## complete, single, average, and median linkage_methods are basically the same idea,
    ## just different summary distance to cluster

    cluster_dist_fun <- switch(linkage_method,
      "single" = min,
      "complete" = max,
      "average" = mean,
      "median" = median
    )

    # need this to be obs on rows, dist to new data on cols
    dists_new <- Rfast::dista(new_data, training_data, trans = TRUE)

    cluster_dists <- bind_cols(data.frame(dists_new), clusters) %>%
      group_by(.cluster) %>%
      summarize_all(cluster_dist_fun)

    pred_clusts_num <- cluster_dists %>%
      select(-.cluster) %>%
      purrr::map_dbl(which.min)

  } else if (linkage_method == "centroid") {

    ## Centroid linkage_method, dist to center

    cluster_centers <- extract_centroids(object) %>% select(-.cluster)
    dists_means <- Rfast::dista(new_data, cluster_centers)

    pred_clusts_num <- apply(dists_means, 1, which.min)

  # } else if (linkage_method %in% c("ward.D", "ward", "ward.D2")) {
  #
  #   ## Ward linkage_method: lowest change in ESS
  #   ## dendrograms created from already-squared distances
  #   ## use Ward.D2 on these plain distances for Ward.D
  #
  #   cluster_centers <- extract_centroids(object)
  #   cluster_centers <- as.matrix(cluster_centers[, -1])
  #
  #   d_means <- purrr::map(1:nrow(cluster_centers),
  #                         ~t(t(training_data) - cluster_centers[.x, ]))
  #
  #   n <- nrow(training_data)
  #
  #   d_new_list <- purrr::map(1:nrow(new_data),
  #                            ~ training_data - new_data[.x,])
  #
  #   change_in_ess <- purrr::map(d_new_list,
  #                               function(v) {
  #                                 purrr::map_dbl(d_means,
  #                                           ~ sum((n*.x + v)^2/(n+1)^2 - .x^2)
  #                                 )}
  #   )
  #
  #   pred_clusts_num <- purrr::map_dbl(change_in_ess, which.min)

  } else {

    stop(glue::glue("linkage_method {linkage_method} is not supported for prediction."))

  }


  pred_clusts <- unique(clusters$.cluster)[pred_clusts_num]

  return(factor(pred_clusts))

}
