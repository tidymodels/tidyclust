# reconciliation works with uneven numbers

    Code
      reconcile_clusterings_mapping(primary_cluster_assignment,
        alt_cluster_assignment, one_to_one = TRUE)
    Condition
      Error in `reconcile_clusterings_mapping()`:
      ! For one-to-one matching, must have the same number of clusters in primary and alt.

# reconciliation errors for uneven lengths

    Code
      reconcile_clusterings_mapping(letters, letters[1:10])
    Condition
      Error in `reconcile_clusterings_mapping()`:
      ! `primary` (26) and `alternative` (10) must be the same length.

