#' Calculates SSE in each cluster
#'
#' @param .model a fitted kmeans celery model
#' @param distance A choice of distance metric to use for computing error.
#' Currently only "euclidean".
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#' cluster.
#'
#' @export

within_cluster_sse <- function(.model, distance = "euclidean") {

  #if engine is stats

  # need to line up new names with old ones
  # unique is in order of appearance and both clusters assignments
  ## are in observatoin order
  res <- tibble(
    .cluster = unique(extract_cluster_assignment(.model)$.cluster),
    orig_label = unique(.model$fit$cluster)
  ) %>%
    arrange(orig_label) %>%
    mutate(
      sse = .model$fit$withinss
    ) %>%
    arrange(.cluster) %>%
    select(-orig_label)

  return(res)

}

#' Calculates SSE in each cluster
#'
#' @param .model A fitted kmeans celery model
#' @param distance A choice of distance metric to use for computing error.
#' Currently only "euclidean".
#'
#' @return A double; the sum of within-cluster sse.
#'
#' @export

wss <- function(.model, distance = "euclidean") {

  ### if engine is stats and model is kmeans and dist is euclidean

  within_ss <- .model$fit$tot.withinss

  return(within_ss)

}

#' Calculates SSE in each cluster
#'
#' @param .model A fitted kmeans celery model
#' @param distance A choice of distance metric to use for computing error.
#' Currently only "euclidean".
#'
#' @return A double; the ratio of within-cluster sse to total (null) sse.
#'
#' @export
sse_ratio <- function(.model, distance = "euclidean") {

  return(wss(.model)/.model$fit$totss)

}



## Silhouette


## Gap method


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
  vec <- data %>% pull({{var}})

  if (!is.numeric(vec)) {

    res <- data %>%
      janitor::tabyl({{clusters}}, {{var}}) %>%
      select(-1) %>%
      as.matrix() %>%
      chisq.test() %>%
      tidy()

  } else {

    ### anova

  }


  return(-log(res$p.value))

}
