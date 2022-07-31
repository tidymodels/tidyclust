test_that("reconciliation works with one-to-one", {

  primary_cluster_assignment <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
  alt_cluster_assignment <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Fish")


  res <- reconcile_clusterings(primary_cluster_assignment, alt_cluster_assignment)

  expect_equal(res$alt_recoded, c("Apple", "Apple", "Carrot", "Apple", "Banana", "Banana"))
})



test_that("reconciliation works with uneven numbers", {

  primary_cluster_assignment <- c("Apple", "Apple", "Carrot", "Carrot", "Banana", "Banana")
  alt_cluster_assignment <- c("Dog", "Dog", "Cat", "Dog", "Parrot", "Fish")


  res <- reconcile_clusterings(primary_cluster_assignment, alt_cluster_assignment, one_to_one = FALSE)

  expect_equal(res$alt_recoded, c("Apple", "Apple", "Carrot", "Apple", "Banana", "Banana"))
})
