library(RcppHungarian)

reconcile_clusterings <- function(clusters_1, clusters_2) {

  clusters_1 <- factor(clusters_1)
  clusters_2 <- factor(clusters_2)

  cost <- as.matrix(table(clusters_1, clusters_2))
  matches <- HungarianSolver(-cost)

  c1_names <- unique(clusters_1)
  c2_new <- factor(clusters_2,
                   levels = levels(clusters_2)[matches$pairs[,2]],
                   labels = levels(clusters_1))

  res <- tibble::tibble(
    clusters_1 = clusters_1,
    clusters_2 = clusters_2,
    clusters_2_renamed = c2_new
  )

  res

}

clusters_1 <- c("C1", "C1", "C2", "C3", "C4", "C4")
clusters_2 <- c("C3", "C2", "C2", "C4", "C1", "C1")

reconcile_clusterings(clusters_1, clusters_2)

km_1 <- kmeans(ir, 10)
km_2 <- kmeans(ir, 10)

bob <- reconcile_clusterings(km_1$cluster, km_2$cluster)
sum(bob$clusters_1 == bob$clusters_2_renamed)
