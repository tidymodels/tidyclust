test_that("reconcile_clusterings_mapping() works with one-to-one", {
  primary_cluster_assignment <- c(
    "Apple",
    "Apple",
    "Carrot",
    "Carrot",
    "Banana",
    "Banana"
  )
  alt_cluster_assignment <- c("Dog", "Dog", "Cat", "Dog", "Fish", "Fish")

  res <- reconcile_clusterings_mapping(
    primary_cluster_assignment,
    alt_cluster_assignment
  )

  expect_equal(
    res$alt_recoded,
    c("Carrot", "Carrot", "Banana", "Carrot", "Apple", "Apple")
  )
})

test_that("reconcile_clusterings_mapping() works with uneven numbers", {
  primary_cluster_assignment <- c(
    "Apple",
    "Apple",
    "Carrot",
    "Carrot",
    "Banana",
    "Banana"
  )
  alt_cluster_assignment <- c("Dog", "Dog", "Cat", "Dog", "Parrot", "Fish")

  expect_snapshot(
    error = TRUE,
    reconcile_clusterings_mapping(
      primary_cluster_assignment,
      alt_cluster_assignment,
      one_to_one = TRUE
    )
  )

  res <- reconcile_clusterings_mapping(
    primary_cluster_assignment,
    alt_cluster_assignment,
    one_to_one = FALSE
  )

  expect_equal(
    res$alt_recoded,
    c("Apple", "Apple", "Carrot", "Apple", "Banana", "Banana")
  )
})

test_that("reconcile_clusterings_mapping() errors for uneven lengths", {
  expect_snapshot(
    error = TRUE,
    reconcile_clusterings_mapping(
      letters,
      letters[1:10]
    )
  )
})

test_that("one_to_one mapping is bijective", {
  skip_if_not_installed("RcppHungarian")

  primary <- c("A", "A", "B", "B", "C", "C")
  alt <- c("X", "X", "Y", "Y", "Z", "Z")

  res <- reconcile_clusterings_mapping(primary, alt, one_to_one = TRUE)

  recoded_levels <- unique(res$alt_recoded)

  expect_identical(length(recoded_levels), length(unique(primary)))
  expect_identical(sort(recoded_levels), sort(unique(primary)))
})

test_that("mapping maximizes agreement", {
  skip_if_not_installed("RcppHungarian")

  primary <- c("A", "A", "A", "B", "B", "C")
  alt <- c("X", "X", "Y", "Y", "Y", "Z")

  res <- reconcile_clusterings_mapping(primary, alt, one_to_one = FALSE)

  agreement <- sum(res$primary == res$alt_recoded)

  expect_gte(agreement, 4)
})
