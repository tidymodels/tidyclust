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
  vec <- data %>% dplyr::pull({{ var }})

  if (!is.numeric(vec)) {
    res <- data %>%
      janitor::tabyl({{ clusters }}, {{ var }}) %>%
      dplyr::select(-1) %>%
      as.matrix() %>%
      stats::chisq.test() %>%
      tidy()
  } else {

    ### anova
  }


  return(-log(res$p.value))
}
