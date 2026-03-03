make_predictions <- function(x, prefix, n_clusters) {
  levels <- seq_len(n_clusters)
  factor(x, levels = levels, labels = paste0(prefix, levels))
}

.k_means_predict_stats <- function(object, new_data, prefix = "Cluster_") {
  res <- object$centers
  res <- flexclust::dist2(res, new_data)
  res <- apply(res, 2, which.min)

  make_predictions(res, prefix, length(object$size))
}

.k_means_predict_ClusterR <- function(object, new_data, prefix = "Cluster_") {
  clusters <- predict(object, new_data)
  n_clusters <- length(object$obs_per_cluster)

  make_predictions(clusters, prefix, n_clusters)
}

.k_means_predict_clustMixType <- function(
  object,
  new_data,
  prefix = "Cluster_"
) {
  clusters <- predict(object, new_data)$cluster
  n_clusters <- length(object$size)

  make_predictions(clusters, prefix, n_clusters)
}

.k_means_predict_klaR <- function(
  object,
  new_data,
  prefix = "Cluster_",
  ties = c("first", "last", "random")
) {
  ties <- rlang::arg_match(ties)

  modes <- object$modes
  n_modes <- nrow(modes)

  clusters <- integer(nrow(new_data))

  modes <- as.matrix(modes)
  new_data <- as.matrix(new_data)

  for (i in seq_along(clusters)) {
    misses <- rowSums(new_data[rep(i, n_modes), ] != modes)

    which_min <- which(misses == min(misses))

    if (length(which_min) == 1) {
      clusters[i] <- which_min
    } else {
      clusters[i] <- switch(
        ties,
        first = which_min[1],
        last = which_min[length(which_min)],
        random = sample(which_min, 1)
      )
    }
  }

  make_predictions(clusters, prefix, n_modes)
}

.hier_clust_predict_stats <- function(
  object,
  new_data,
  ...,
  prefix = "Cluster_"
) {
  linkage_method <- object$method

  new_data <- as.matrix(new_data)

  training_data <- as.matrix(attr(object, "training_data"))
  clusters <- extract_cluster_assignment(
    object,
    ...,
    prefix = prefix,
    call = call("predict")
  )

  if (linkage_method %in% c("single", "complete", "average", "median")) {
    ## complete, single, average, and median linkage_methods are basically the
    ## same idea, just different summary distance to cluster

    cluster_dist_fun <- switch(
      linkage_method,
      "single" = min,
      "complete" = max,
      "average" = mean,
      "median" = stats::median
    )

    # need this to be obs on rows, dist to new data on cols
    dists_new <- philentropy::dist_many_many(
      training_data,
      new_data,
      method = "euclidean"
    )

    cluster_dists <- dplyr::bind_cols(data.frame(dists_new), clusters) |>
      dplyr::group_by(.cluster) |>
      dplyr::summarize_all(cluster_dist_fun)

    pred_clusts_num <- cluster_dists |>
      dplyr::select(-.cluster) |>
      map_dbl(which.min)
  } else if (linkage_method == "centroid") {
    ## Centroid linkage_method, dist to center

    cluster_centers <- extract_centroids(object) |> dplyr::select(-.cluster)

    dists_means <- philentropy::dist_many_many(
      new_data,
      cluster_centers,
      method = "euclidean"
    )

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
      \(.x)
        t(
          t(training_data[clusters$.cluster == cluster_names[.x], ]) -
            cluster_centers[.x, ]
        )
    )

    d_new_list <- map(
      seq_len(nrow(new_data)),
      function(new_obs) {
        map(
          seq_len(n_clust),
          \(.x)
            t(
              t(training_data[clusters$.cluster == cluster_names[.x], ]) -
                new_data[new_obs, ]
            )
        )
      }
    )

    n <- nrow(training_data)

    change_in_ess <- map(
      d_new_list,
      function(v) {
        map2_dbl(
          d_means,
          v,
          \(.x, .y) sum((n * .x + .y)^2 / (n + 1)^2 - .x^2)
        )
      }
    )

    pred_clusts_num <- map_dbl(change_in_ess, which.min)
  } else {
    cli::cli_abort(
      "linkage_method {.val {linkage_method}} is not supported for prediction."
    )
  }
  pred_clusts <- unique(clusters$.cluster)[pred_clusts_num]

  pred_clusts
}

itemsets_predict_helper <- function(object, new_data, ..., prefix = "Cluster_") {
  new_data <- as.data.frame(new_data)

  # Extract frequent itemsets and their supports
  items <- attr(object, "item_names")
  itemsets <- arules::DATAFRAME(object)
  frequent_itemsets <- lapply(strsplit(gsub("[{}]", "", itemsets$items), ","), trimws)
  supports <- itemsets$support

  # Calculate global support for each item (fallback)
  global_supports <- sapply(items, function(item) {
    containing <- sapply(frequent_itemsets, function(x) item %in% x)
    if (any(containing)) {
      sum(supports[containing]) / sum(containing)
    } else {
      0
    }
  })

  # Process each row of new_data
  result_list <- lapply(1:nrow(new_data), function(i) {
    row_data <- new_data[i, ]
    observed <- names(row_data)[row_data == 1]
    missing <- names(row_data)[is.na(row_data)]

    # Initialize prediction vector
    pred_values <- rep(NA, length(items))
    names(pred_values) <- items

    # Calculate probabilities for missing items
    for (item in missing) {
      # Find itemsets containing both the current item and at least one observed item
      relevant <- sapply(frequent_itemsets, function(x) {
        item %in% x && any(observed %in% x)
      })

      if (!any(relevant)) {
        pred_values[item] <- global_supports[item]
        next
      }

      # Calculate confidences
      confidences <- sapply(which(relevant), function(idx) {
        itemset <- frequent_itemsets[[idx]]
        itemset_without <- setdiff(itemset, item)

        # Find support of itemset without the current item
        if (length(itemset_without) == 0) return(NA)

        matches <- sapply(frequent_itemsets, function(x) identical(x, itemset_without))
        if (!any(matches)) return(NA)

        supports[idx] / supports[matches][1]
      })

      pred_values[item] <- mean(confidences, na.rm = TRUE)
      if (is.nan(pred_values[item])) pred_values[item] <- global_supports[item]
    }

    # Create result data frame
    data.frame(
      item = gsub("`", "", items), # Remove backticks from item names
      .obs_item = unlist(row_data),
      .pred_item = pred_values,
      row.names = NULL
    )
  })
}

.freq_itemsets_predict_raw_arules <- function(object, new_data, ..., prefix = "Cluster_") {
  res <- itemsets_predict_helper(object, new_data, ..., prefix = "Cluster_")
  return(tibble::tibble(.pred_cluster = unname(res)))
}

.freq_itemsets_predict_arules <- function(object, new_data, ..., prefix = "Cluster_") {
  res <- itemsets_predict_helper(object, new_data, ..., prefix = "Cluster_")
  # Apply threshold to raw predictions
  lapply(res, function(df) {
    df$.pred_item <- ifelse(is.na(df$.obs_item),
                            ifelse(df$.pred_item >= 0.5, 1, 0),
                            NA)
    df
  })
}
