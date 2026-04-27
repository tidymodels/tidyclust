# finalize_model_tidyclust() errors on non-cluster_spec

    Code
      finalize_model_tidyclust("not a spec", data.frame(num_clusters = 5))
    Condition
      Error in `finalize_model_tidyclust()`:
      ! `x` should be a tidyclust model specification.

# finalize_model_tidyclust() is deprecated

    Code
      . <- finalize_model_tidyclust(spec, params)
    Condition
      Warning:
      `finalize_model_tidyclust()` was deprecated in tidyclust 0.3.0.
      i Please use `tune::finalize_model()` instead.

# finalize_workflow_tidyclust() errors on non-workflow

    Code
      finalize_workflow_tidyclust("not a workflow", data.frame(num_clusters = 5))
    Condition
      Error in `finalize_workflow_tidyclust()`:
      ! `x` should be a string

# finalize_workflow_tidyclust() is deprecated

    Code
      . <- finalize_workflow_tidyclust(wf, params)
    Condition
      Warning:
      `finalize_workflow_tidyclust()` was deprecated in tidyclust 0.3.0.
      i Please use `tune::finalize_workflow()` instead.
      Warning:
      `finalize_model_tidyclust()` was deprecated in tidyclust 0.3.0.
      i Please use `tune::finalize_model()` instead.

