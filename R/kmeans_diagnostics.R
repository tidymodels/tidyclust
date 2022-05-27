#-------- SSE -------#

#' Calculates Sum of Squared Error in each cluster
#'
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#' cluster.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   within_cluster_sse()
#'
#' @import dplyr
#'
#' @export
within_cluster_sse <- function(object, new_data = NULL,
                               dist_fun = Rfast::dista) {


  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {

    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)

  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {

    res <- tibble::tibble(
      .cluster = factor(summ$cluster_names),
      wss = summ$within_sse
      )

  } else {

    dist_to_centroids <- dist_fun(summ$centroids, new_data)

    res <- dist_to_centroids %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      purrr::map_dfr(~c(.cluster = which.min(.x),
                 dist = min(.x)^2)) %>%
      mutate(
        .cluster = factor(paste0("Cluster_", .cluster))
      ) %>%
      group_by(.cluster) %>%
      summarize(wss = sum(dist))

  }

  return(res)

}


#' Compute the sum of within-cluster SSE
#'
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_wss()
#' @export
tot_wss <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {

  sum(within_cluster_sse(object, new_data, dist_fun, ...)$wss, na.rm = TRUE)

}

#' Compute the total sum of squares
#'
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_sse()
#' @export
tot_sse <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {


  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {

    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)

  }

  summ <- extract_fit_summary(object)

  if (is.null(new_data)) {
    tot <- summ$tot_sse
  } else {
    overall_mean <- colSums(summ$centroids * summ$n_members)/sum(summ$n_members)
    tot <- dist_fun(t(as.matrix(overall_mean)), new_data)^2 %>% sum()
  }

  return(tot)

}



#' Compute the ratio of the WSS to the total SSE
#'
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dist_fun A function for calculating distances to centroids.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   sse_ratio()
#' @export
sse_ratio <- function(object, new_data = NULL, dist_fun = Rfast::dista, ...) {

  tot_wss(object, new_data, dist_fun)/tot_sse(object, new_data, dist_fun)

}



#-------- Silhouette -------#

#' Measures silhouettes between clusters
#'
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dists A distance matrix. Used if `new_data` is `NULL`.
#' @param dist_fun A function for calculating distances between observations.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A tibble giving the silhouettes for each observation.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars %>%
#'   as.matrix() %>%
#'   dist()
#'
#' silhouettes(kmeans_fit, dists)
#'
#' @export
silhouettes <- function(object, new_data = NULL,
                        dists = NULL, dist_fun = Rfast::Dist) {

  preproc <- prep_data_dist(object, new_data, dists, dist_fun)

  clust_int <- as.integer(gsub("Cluster_", "", preproc$clusters))

  sil <- cluster::silhouette(clust_int, preproc$dists)

  sil %>%
    unclass() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      cluster = factor(paste0("Cluster_", cluster)),
      neighbor = factor(paste0("Cluster_", neighbor)),
      sil_width = as.numeric(sil_width)
    )

}


#' Measures average silhouette across all observations
#' @param object A fitted kmeans celery model
#' @param new_data A dataset to predict on.  If `NULL`, uses trained clustering.
#' @param dists A distance matrix. Used if `new_data` is `NULL`.
#' @param dist_fun A function for calculating distances between observations.  Defaults
#' to Euclidean distance on processed data.
#' @param ... Other arguments passed to methods.
#'
#' @return A double; the average silhouette.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' dists <- mtcars %>%
#'   as.matrix() %>%
#'   dist()
#'
#' avg_silhouette(kmeans_fit, dists)
#'
#' @export
avg_silhouette <- function(object, new_data = NULL,
                           dists = NULL, dist_fun = Rfast::Dist,
                           ...) {

  mean(silhouettes(object, new_data, dists, dist_fun, ...)$sil_width)

}

#-------- Gap Method -------#

#### Not sure whether to add this, it's basically a resampling method


#-------- Enrichment -------#


#### This one needs to change to fit the new structure, not a priority for now

#' Measures relationship between cluster assignments and another categorical variable.
#'
#' @param data the dataset
#' @param clusters the variable with cluster assignments
#' @param var other variables for enrichment
#'
#' @return The p-value of a Chi-Square test for relationship between cluster
#' assignments and the categorical variable.

# this needs to be ... instead of var soon. change @param too when it happens
#' @export
enrichment <- function(data, clusters, var) {

  res <- list()
  vec <- data %>% dplyr::pull({{var}})

  if (!is.numeric(vec)) {

    res <- data %>%
      janitor::tabyl({{clusters}}, {{var}}) %>%
      dplyr::select(-1) %>%
      as.matrix() %>%
      stats::chisq.test() %>%
      tidy()

  } else {

    ### anova

  }


  return(-log(res$p.value))

}

#------ Helpers ----- #

#' Prepares data and distance matrices for metric calculation
#'
#' @param object A fitted cluster_spec object.
#' @param new_data A dataset to calculate predictions on.  If `NULL`, the trained
#' cluster assignments from the fitted object are used.
#' @param dists A distance matrix for the data.  If `NULL`, distance is computed
#' on `new_data` using the `stats::dist()` function.
#' @param ... Optional parameters passed to `stats::dists()`
#'
#' @return A list

prep_data_dist <- function(object, new_data = NULL,
                           dists = NULL, dist_fun = Rfast::Dist) {

  # Sihouettes requires a distance matrix
  if (is.null(new_data) & is.null(dists)) {
    stop("Must supply either a dataset or distance matrix to compute silhouettes.")
  }

  # If data is blank, we are using the trained cluster assignments
  if (is.null(new_data)) {
    clusters <- extract_fit_summary(object)$cluster_assignments
  } else {
    clusters <- predict_cluster(object, new_data)
  }


  # If they supplied distance, check that it matches the data dimension
  if (!is.null(dists)) {
    if (!is.null(new_data) && nrow(new_data) != attr(dists, "Size")) {
      stop("Dimensions of dataset and distance matrix must match.")
    } else if (is.null(new_data) && length(clusters) != attr(dists, "Size")) {
      stop("Dimensions of training dataset and distance matrix must match.")
    }
  }

  # Preprocess data before computing distances if appropriate
  if (inherits(object, "workflow") && !is.null(new_data)) {

    new_data <- object %>%
      hardhat::extract_recipe() %>%
      recipes::bake(new_data)

  }

  # Calculate distances including optionally supplied params
  if (is.null(dists)) {
    dists <- dist_fun(new_data)
  }

  return(list(clusters = clusters,
              data = new_data,
              dists = dists))

}



#' Computes distance from observations to centroids
#'
#' @param new_data A data frame
#' @param centroids A data frame where each row is a centroid.
#' @param dist_fun A function for computing matrix-to-matrix distances.
#' Defaults to `Rfast::dista()`
#'
get_centroid_dists <- function(new_data, centroids,
                               dist_fun = Rfast::dista) {

  if (ncol(new_data) != ncol(centroids)) {
    stop("Centroids must have same columns as data.")
  }

  dist_fun(centroids, new_data)


}


