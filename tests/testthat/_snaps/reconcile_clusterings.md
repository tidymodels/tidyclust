# reconciliation works with uneven numbers

    Code
      reconcile_clusterings_mapping(primary_cluster_assignment,
        alt_cluster_assignment, one_to_one = TRUE)
    Error <rlang_error>
      For one-to-one matching, must have the same number of clusters inprimary and alt.

# reconciliation errors for uneven lengths

    Code
      reconcile_clusterings_mapping(letters, letters[1:10])
    Error <rlang_error>
      `primary` (26) and `alternative` (10) must be the same length.

