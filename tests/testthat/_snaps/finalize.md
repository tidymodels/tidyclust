# finalize_model_tidyclust() errors on non-cluster_spec

    Code
      finalize_model_tidyclust("not a spec", data.frame(num_clusters = 5))
    Condition
      Error in `finalize_model_tidyclust()`:
      ! `x` should be a tidyclust model specification.

# finalize_workflow_tidyclust() errors on non-workflow

    Code
      finalize_workflow_tidyclust("not a workflow", data.frame(num_clusters = 5))
    Condition
      Error in `finalize_workflow_tidyclust()`:
      ! `x` should be a string

