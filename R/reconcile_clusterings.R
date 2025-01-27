#' Relabels clusters to match another cluster assignment
#'
#' Retains the cluster labels of the primary assignment, and relabel the
#' alternate assignment to match as closely as possible.  The user must decide
#' whether clusters are forced to be "one-to-one"; that is, are we allowed to
#' assign multiple labels from the alternate assignment to the same primary
#' label?
#'
#' @param primary A vector containing cluster labels, to be
#'   matched
#' @param alternative Another vector containing cluster labels, to be
#'   changed
#' @param one_to_one Boolean; should each alt cluster match only one primary
#'   cluster?
#' @param optimize One of "accuracy" or "precision"; see description.
#'
#' @description
#' When forcing one-to-one, the user needs to decide what to prioritize:
#'  *  "accuracy": optimize raw count of all observations with the same label
#' across the two assignments
#'  *  "precision": optimize the average percent of each alt cluster that
#' matches the corresponding primary cluster
#'
#' @return A tibble with 3 columns; `primary`, `alt`, `alt_recoded`
#' @examplesIf rlang::is_installed("RcppHungarian")
#' factor1 <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
#' factor2 <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Fish")
#' reconcile_clusterings_mapping(factor1, factor2)
#'
#' factor1 <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
#' factor2 <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Parrot")
#' reconcile_clusterings_mapping(factor1, factor2, one_to_one = FALSE)
#' @export
reconcile_clusterings_mapping <- function(
  primary,
  alternative,
  one_to_one = TRUE,
  optimize = "accuracy"
) {
  rlang::check_installed("RcppHungarian")
  if (length(primary) != length(alternative)) {
    cli::cli_abort(
      "{.arg primary} ({length(primary)}) and {.arg alternative} ({length(alternative)})
      must be the same length."
    )
  }

  clusters_1 <- as.factor(primary)
  clusters_2 <- as.factor(alternative)

  nclust_1 <- length(levels(clusters_1))
  nclust_2 <- length(levels(clusters_2))

  if (one_to_one && nclust_1 != nclust_2) {
    cli::cli_abort(
      "For one-to-one matching, must have the same number of clusters in 
      primary and alt."
    )
  } else if (nclust_1 > nclust_2) {
    cli::cli_abort(
      "Primary clustering must have equal or fewer clusters to alternate
      clustering."
    )
  }

  ## Use standard names in order for both
  clusters_1_f <- factor(
    x = clusters_1,
    labels = paste0("Cluster_", seq_len(nclust_1))
  )
  clusters_2_f <- factor(
    x = clusters_2,
    labels = paste0("Cluster_", seq_len(nclust_2))
  )
  clusters_1_f <- factor(
    x = clusters_1_f,
    levels = levels(clusters_2_f)
  )

  ## Get counts
  cross_counts <- table(clusters_1_f, clusters_2_f)
  cross_counts <- matrix(
    cross_counts,
    ncol = ncol(cross_counts),
    dimnames = dimnames(cross_counts)
  )

  ## one-to-one and accuracy = hungarian on counts
  ## one-to-one and precision = hungarian on col-stdized

  if (one_to_one) {
    ## Hungarian solver guarantees max diagonal sum
    cost <- cross_counts

    if (optimize == "precision") {
      cost <- t(t(cost) / colSums(cost))
    }

    matches <- RcppHungarian::HungarianSolver(-cost)
    reord <- matches$pairs[, 2]
  } else {
    reord <- c(apply(cross_counts, 2, which.max))
  }

  ## Reorder new clusters and then use original labels

  recode_vec <- levels(clusters_2)
  names(recode_vec) <- levels(clusters_1)[reord]

  c2_new <- factor(
    names(recode_vec)[as.integer(clusters_2)],
    levels = unique(names(recode_vec))
  )

  tibble::tibble(
    primary = primary,
    alt = alternative,
    alt_recoded = as.character(c2_new)
  )
}
