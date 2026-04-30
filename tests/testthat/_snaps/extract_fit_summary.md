# extract_fit_summary() errors for cluster spec

    Code
      extract_fit_summary(spec)
    Condition
      Error in `extract_fit_summary()`:
      ! This function requires a fitted model.
      i Please use `fit()` on your cluster specification.

# labels length mismatch errors in extract_fit_summary()

    Code
      extract_fit_summary(spec, labels = c("A", "B"))
    Condition
      Error in `make_cluster_labels()`:
      ! `labels` must have length 3, not 2.

# duplicate labels errors in extract_fit_summary()

    Code
      extract_fit_summary(spec, labels = c("A", "A", "B"))
    Condition
      Error in `make_cluster_labels()`:
      ! `labels` must not contain duplicate values. Duplicated: "A".

