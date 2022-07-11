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

  method <- object$fit$method

  old_data <- attr(object, "training_data")
  clusters <- extract_cluster_assignment(object)

  if (method %in% c("single", "complete", "average", "median")) {

    ## complete, single, average, and median methods are basically the same idea,
    ## just different summary distance to cluster

    cluster_dist_fun <- switch(method,
      "single" = min,
      "complete" = max,
      "average" = mean,
      "median" = median
    )

    # need this to be obs on rows, dist to new data on cols
    dists_new <- fastR::dist(new_data, training_data)

    cluster_dists <- bind_cols(dists_new, clusters) %>%
      group_by(.cluster) %>%
      summarize(cluster_dist_fun)

    pred_clusts_num <- apply(cluster_dists, 2, which.min)

  } else if (method == "centroid") {

    ## Centroid method, dist to center

    cluster_centers <- extract_centroids(object)
    dists_means <- fastR::dist(new_data, cluster_centers)

    pred_clusts_num <- apply(dists_means, 2, which.min)

  } else if (method %in% c("ward.D", "ward", "ward.D2")) {

    ## Ward method: lowest change in ESS
    ## dendrograms created from already-squared distances
    ## use Ward.D2 on these plain distances for Ward.D

    cluster_centers <- extract_centroids(object)

    d_means <- purrr::map(1:nrow(cluster_centers),
                          ~training_data - .x)

    n <- nrow(training_data)

    d_new_list <- purrr::map(1:nrow(new_data),
                             ~ training_data - new_data[.x,])

    change_in_ess <- purrr::map(d_new_list,
                                function(v) {
                                  purrr_map_dbl(d_means,
                                            ~ sum((n*.x + v2)^2/(n+1)^2 - .x^2)
                                  )}
    )

    pred_clusts_num <- purrr::map_dbl(change_in_ess, which.min)

  } else {

    stop(glue::glue("Method {method} is not supported for prediction."))

  }


  pred_clusts <- clusters$.cluster[pred_clusts_num]

  return(pred_clusts)


}
