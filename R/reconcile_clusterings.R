#' Relabels clusters to match another cluster assignment
#'
#' Retains the cluster labels of the primary assignment, and relabel the alternate assignment
#'  to match as closely as possible.  The user must decide whether clusters are forced to be
#'  "one-to-one"; that is, are we allowed to assign multiple labels from the alternate assignment
#'  to the same primary label?
#'
#'  The user can opt to prioritize:
#'  *  "accuracy": Each alt label will be assigned to the primary label with which
#'  it shares the highest raw member count.
#'  *  "precision": Each alt label will be assigned to the primary label that captures
#'  the highest percentage of its members.
#'  *  "recall":  Each alt label will be assigned
#'
#' @param primary_cluster_assignment A vector containing cluster labels, to be matched
#' @param alt_cluster_assignment Another vector containing cluster labels, to be changed
#' @param one_to_one Boolean; should each alt cluster match only one primary cluster?
#' @param optimize One of "precision", "recall" or "accuracy"; see description.
#'
#' @return A vector with the new cluster labels
#'
#' @importFrom forcats fct_inorder

reconcile_clusterings <- function(primary_cluster_assignment,
                                  alt_cluster_assignment,
                                  one_to_one = TRUE,
                                  optimize = ) {


  clusters_1 <- fct_inorder(as.character(primary_cluster_assignment))
  clusters_2 <- fct_inorder(as.character(alt_cluster_assignment))

  nclust_1 <- length(levels(clusters_1))
  nclust_2 <- length(levels(clusters_2))


  if (nclust_1 > nclust_2) {
    stop("Primary clustering must have equal or fewer clusters to alternate clustering.")
  }

  ## Use standard names in order for both

  clusters_1_f <- factor(clusters_1, labels = paste0("Cluster_", 1:nclust_1))
  clusters_2_f <- factor(clusters_2, labels = paste0("Cluster_", 1:nclust_2))
  clusters_1_f <- factor(clusters_1_f, levels = levels(clusters_2_f))


  ## Get counts
  cost <- as.matrix(table(clusters_1_f, clusters_2_f))
  cost <- t(t(cost)/colSums(cost))

  ## If they have the same size, it's easy
  ## If there are more clusters in the alt clustering, try all combos

  if (nclust_1 == nclust_2) {

    matches <- RcppHungarian::HungarianSolver(-cost)
    reord <- matches$pairs[,2]

  } else {


    n_combos <- choose(nclust_2, nclust_1)

  }


  ## Reorder new clusters and then use original labels

  c2_new <- factor(clusters_2_f,
                   levels = levels(clusters_2_f)[],
                   labels = levels(clusters_1))



  return(as.character(c2_new))

}

primary_cluster_assignment <- c("C4", "C4", "C3", "C3", "C1", "C1")
alt_cluster_assignment <- c("C3", "C2", "C2", "C4", "C1", "C1")

reconcile_clusterings(clusters_1, clusters_2)

## test it with numbers

primary_cluster_assignment <- sample(1:10, 100, replace = TRUE)
alt_cluster_assignment <- sample(1:10, 100, replace = TRUE)

thing <- tibble(
 a =  primary_cluster_assignment,
 b= alt_cluster_assignment,
 c= reconcile_clusterings(primary_cluster_assignment, alt_cluster_assignment)
)

table(thing$a, thing$c)


## test it with too many in c2

clusters_1 <- sample(1:3, 10, replace = TRUE)
clusters_2 <- sample(1:4, 10, replace = TRUE)

cbind(
  clusters_1,
  clusters_2,
  reconcile_clusterings(clusters_1, clusters_2)
)

### what happens when clusters2 and clusters1 don't share names, even permuted?
### or not the same number of unique values?
