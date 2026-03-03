#' Extract cluster assignments from model
#'
#' When applied to a fitted cluster specification, returns a tibble with cluster
#' assignments of the data used to train the model.
#'
#' @param object An fitted [`cluster_spec`] object.
#' @param ... Other arguments passed to methods. Using the `prefix` allows you
#'   to change the prefix in the levels of the factor levels.
#'
#' @details
#'
#' Some model types such as K-means as seen in [k_means()] stores the
#' cluster assignments in the object itself. leading the use of this function to
#' act as an simple extract. Other model types such as Hierarchical
#' (Agglomerative) Clustering as seen in [hier_clust()], are fit in such a way
#' that the number of clusters can be determined at any time after the fit.
#' Setting the `num_clusters` or `cut_height` in this function will be used to
#' determine the clustering when reported.
#'
#' The ordering of the clusters is such that the first observation in the
#' training data set will be in cluster 1, the next observation that doesn't
#' belong to cluster 1 will be in cluster 2, and so on and forth. As the
#' ordering of clustering doesn't matter, this is done to avoid identical sets
#' of clustering having different labels if fit multiple times.
#'
#' ## Related functions
#'
#' `extract_cluster_assignment()` is a part of a trio of functions doing
#' similar things:
#'
#' - [extract_cluster_assignment()] returns the cluster assignments of the
#'   training observations
#' - [extract_centroids()] returns the location of the centroids
#' - \code{\link[=predict.cluster_fit]{predict()}} returns the cluster a new
#'   observation belongs to
#'
#' @return A `tibble::tibble()` with 1 column named `.cluster`. This tibble will
#'   correspond the the training data set.
#'
#' @seealso [extract_centroids()] [predict.cluster_fit()]
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5) |>
#'   set_engine("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit |>
#'   extract_cluster_assignment()
#'
#' kmeans_fit |>
#'   extract_cluster_assignment(prefix = "C_")
#'
#' # Some models such as `hier_clust()` fits in such a way that you can specify
#' # the number of clusters after the model is fit
#' hclust_spec <- hier_clust() |>
#'   set_engine("stats")
#'
#' hclust_fit <- fit(hclust_spec, ~., mtcars)
#'
#' hclust_fit |>
#'   extract_cluster_assignment(num_clusters = 2)
#'
#' hclust_fit |>
#'   extract_cluster_assignment(cut_height = 250)
#' @export
extract_cluster_assignment <- function(object, ...) {
  UseMethod("extract_cluster_assignment")
}

#' @export
extract_cluster_assignment.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
extract_cluster_assignment.cluster_fit <- function(object, ...) {
  extract_cluster_assignment(object$fit, ...)
}

#' @export
extract_cluster_assignment.workflow <- function(object, ...) {
  extract_cluster_assignment(object$fit$fit$fit, ...)
}

#' @export
extract_cluster_assignment.kmeans <- function(object, ...) {
  cluster_assignment_tibble(object$cluster, length(object$size), ...)
}

#' @export
extract_cluster_assignment.KMeansCluster <- function(object, ...) {
  n_clusters <- length(object$obs_per_cluster)
  cluster_assignment_tibble(object$clusters, n_clusters, ...)
}

#' @export
extract_cluster_assignment.kproto <- function(object, ...) {
  n_clusters <- length(object$size)
  cluster_assignment_tibble(object$cluster, n_clusters, ...)
}

#' @export
extract_cluster_assignment.kmodes <- function(object, ...) {
  n_clusters <- length(object$size)
  cluster_assignment_tibble(object$cluster, n_clusters, ...)
}

#' @export
extract_cluster_assignment.hclust <- function(
  object,
  ...,
  call = rlang::caller_env(0)
) {
  # if k or h is passed in the dots, use those.  Otherwise, use attributes
  # from original model specification
  args <- list(...)

  if (!is.null(args[["h"]])) {
    cli::cli_abort(
      c(
        "Using {.arg h} argument is not supported.",
        "i" = "Please use {.arg cut_height} instead."
      ),
      call = call
    )
  }

  if (!is.null(args[["k"]])) {
    cli::cli_abort(
      c(
        "Using {.arg k} argument is not supported.",
        "i" = "Please use {.arg num_clusters} instead."
      ),
      call = call
    )
  }

  if (!("num_clusters" %in% names(args) || "cut_height" %in% names(args))) {
    num_clusters <- attr(object, "num_clusters")
    cut_height <- attr(object, "cut_height")
  } else {
    num_clusters <- args[["num_clusters"]]
    cut_height <- args[["cut_height"]]
  }

  if (is.null(num_clusters) && is.null(cut_height)) {
    cli::cli_abort(
      "Please specify either {.arg num_clusters} or {.arg cut_height}.",
      call = call
    )
  }

  clusters <- stats::cutree(object, k = num_clusters, h = cut_height)
  cluster_assignment_tibble(clusters, length(unique(clusters)), ...)
}

#' @export
extract_cluster_assignment.itemsets <- function(object, ...) {
  max_iter = 1000
  items <- attr(object, "item_names")
  itemsets <- arules::DATAFRAME(object)

  itemset_list <- lapply(strsplit(gsub("[{}]", "", itemsets$items), ","), trimws)
  support <- itemsets$support
  clusters <- numeric(length(items))
  changed <- TRUE  # Flag to track convergence
  iter <- 0 # Initialize iteration counter

  # Continue until no changes occur
  while (changed && iter < max_iter) {
    changed <- FALSE
    iter <- iter + 1
    for (i in 1:length(items)) {
      current_item <- items[i]
      relevant_itemsets <- which(sapply(itemset_list, function(x) current_item %in% x))

      if (length(relevant_itemsets) == 0) next  # Skip if no itemsets

      # Find the best itemset (largest size, then highest support)
      best_itemset <- relevant_itemsets[
        which.max(
          sapply(itemset_list[relevant_itemsets], length) * 1000 +  # Size dominates
          support[relevant_itemsets]                                # Support breaks ties
          )
        ]
      best_itemset_size <- length(itemset_list[[best_itemset]])
      best_itemset_support <- support[best_itemset]

      # Current cluster info (if any)
      current_cluster <- clusters[i]
      current_cluster_size <- if (current_cluster != 0)
        length(itemset_list[[current_cluster]]) else 0
      current_cluster_support <- if (current_cluster != 0)
        support[current_cluster] else 0

      # Reassign if:
      # 1. No current cluster, OR
      # 2. New itemset is larger, OR
      # 3. Same size but higher support
      if (current_cluster == 0 ||
          best_itemset_size > current_cluster_size ||
          (best_itemset_size == current_cluster_size &&
          best_itemset_support > current_cluster_support)) {

        # Assign all items in the best itemset to its cluster
        new_cluster <- best_itemset
        items_in_best <- match(itemset_list[[best_itemset]], items)

        if (!all(clusters[items_in_best] == new_cluster)) {
          clusters[items_in_best] <- new_cluster
          changed <- TRUE  # Mark that a change occurred
        }
      }
    }
  }

  if (iter == max_iter && changed) {
    rlang::warn(
      paste0(
        "Cluster assignment did not converge within the maximum of ",
        max_iter,
        " iterations. Returning the current cluster assignments."
      )
    )
  }

  item_assignment_tibble_w_outliers(clusters, ...)
}

# ------------------------------------------------------------------------------

cluster_assignment_tibble <- function(
  clusters,
  n_clusters,
  ...,
  prefix = "Cluster_"
) {
  reorder_clusts <- order(union(unique(clusters), seq_len(n_clusters)))
  names <- paste0(prefix, seq_len(n_clusters))
  res <- names[reorder_clusts][clusters]

  tibble::tibble(.cluster = factor(res))
}

item_assignment_tibble_w_outliers <- function(clusters,
                                              ...,
                                              prefix = "Cluster_") {
  # Vector to store the resulting cluster names
  res <- character(length(clusters))

  # For items with cluster value 0, assign to "Cluster_0"
  res[clusters == 0] <- "Cluster_0"
  zero_count <- 0
  res <- sapply(res, function(x) {
    if (x == "Cluster_0") {
      zero_count <<- zero_count + 1
      paste0("Cluster_0_", zero_count)
    } else {
      x
    }
  })

  # For non-zero clusters, assign sequential cluster numbers starting from "Cluster_1"
  non_zero_clusters <- clusters[clusters != 0]
  unique_non_zero_clusters <- unique(non_zero_clusters)

  # Map each unique non-zero cluster to a new cluster starting from Cluster_1
  cluster_map <- stats::setNames(paste0(prefix, seq_along(unique_non_zero_clusters)), unique_non_zero_clusters)

  # Assign the corresponding cluster names to the non-zero clusters
  res[clusters != 0] <- cluster_map[as.character(non_zero_clusters)]

  tibble::tibble(.cluster = factor(res))
}
