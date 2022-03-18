#' Calculates SSE in each cluster
#'
#' @param .model a fitted kmeans celery model
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#' cluster.

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

wss <- function(.model, distance = "euclidean") {

  ### if engine is stats

  within_ss <- .model$fit$tot.withinss

  return(within_ss)

}

sse_ratio <- function(.model, distance = "euclidean") {

  return(wss(.model)/.model$fit$totss)

}



## Silhouette


## Gap method


#' Measures correlation between cluster assignments and another variable
#'
#' @param data the dataset
#' @param clusters the variable with cluster assignments
#' @param ...  other variables for enrichment
#'

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
