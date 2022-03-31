#-------- SSE -------#

#' Calculates Sum of Squared Error in each cluster
#'
#' @param object a fitted kmeans celery model
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
#' @export
within_cluster_sse <- function(object, ...) {

  summ <- extract_fit_summary(object)

  res <- tibble::tibble(
    .cluster = unique(extract_cluster_assignment(object)$.cluster),
    orig_label = unique(summ$orig_label)
  ) %>%
    dplyr::arrange(orig_label) %>%
    dplyr::mutate(
      sse = summ$within_sse
    ) %>%
    dplyr::arrange(.cluster) %>%
    dplyr::select(-orig_label)

  return(res)

}


#' Compute the sum of within-cluster SSE
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
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
tot_wss <- function(object, ...) {

  sum(extract_fit_summary(object)$within_sse)

}

#' Compute the total sum of squares
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
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
tot_sse <- function(object, ...) {

  extract_fit_summary(object)$tot_sse

}



#' Compute the ratio of the WSS to the total SSE
#'
#' @param object An cluster_spec object.
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
sse_ratio <- function(object, ...) {

  tot_wss(object)/tot_sse(object)

}



#-------- Silhouette -------#

#' Measures silhouettes between clusters
#'
#' @param .dist A distance matrix
#' @param clusters A vector containing cluster assignments in the
#' row order of the distance matrix.
#'
#' @return The silhouettes matrix.
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
#' silhouettes(dists, kmeans_fit$fit$cluster)
#'
#' @export
silhouettes <- function(.dist, clusters) {

  clust_int <- as.integer(gsub("Cluster_", "", clusters))

  sil <- cluster::silhouette(clust_int, .dist)

  sil %>%
    unclass() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      cluster = factor(paste0("Cluster_", cluster)),
      neighbor = factor(paste0("Cluster_", neighbor)),
      sil_width = as.numeric(sil_width)
    )

}


#' Measures average silhouette between clusters
#' @param .dist A distance matrix
#' @param clusters A vector containing cluster assignments in the
#' row order of the distance matrix.
#'
#' @return The silhouettes matrix.
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
#' avg_silhouette(dists, kmeans_fit$fit$cluster)
#'
#' @export
avg_silhouette <- function(.dist, clusters) {

  mean(silhouettes(.dist, clusters)$sil_width)

}

#-------- Gap Method -------#

#-------- Enrichment -------#

#' Measures relationship between cluster assignments and another categorical variable.
#'
#' @param data the dataset
#' @param clusters the variable with cluster assignments
#' @param ...  other variables for enrichment
#'
#' @return The p-value of a Chi-Square test for relationship between cluster
#' assignments and the categorical variable.

# this needs to be ... instead of var soon
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

