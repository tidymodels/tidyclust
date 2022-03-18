#' Calculates SSE in each cluster
#'
#' @param .model a fitted kmeans celery model
#'

#... exploit what already exists, or re-implement ourselves for any cluster?


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

    res[[1]] <- data %>%
      janitor::tabyl({{clusters}}, {{var}})

    res[[2]] <- res[[1]] %>%
      select(-1) %>%
      as.matrix() %>%
      chisq.test() %>%
      tidy()

  } else {

    res[[1]] <- data %>%
      group_by({{clusters}}) %>%
      summarize(mean = mean({{var}}),
                sd = sd({{var}}))

    res[[2]] <- 0

  }


  return(res)

}
