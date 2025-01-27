# cluster_metric_set works

    Code
      my_metrics(kmeans_fit)
    Condition
      Error in `value[[3L]]()`:
      ! In metric: `silhouette_avg` Must supply either a dataset or distance matrix to compute silhouettes.

# cluster_metric_set error with wrong input

    Code
      cluster_metric_set(mean)
    Condition
      Error in `validate_function_class()`:
      ! The combination of metric functions must be only clustering metrics.
      i The following metric function types are being mixed:
      - other (mean <namespace:base>)

---

    Code
      cluster_metric_set(sse_ratio, mean)
    Condition
      Error in `validate_function_class()`:
      ! The combination of metric functions must be only clustering metrics.
      i The following metric function types are being mixed:
      - cluster (sse_ratio)
      - other (mean <namespace:base>)

# cluster_metric_set errors with advice for some functions

    Code
      cluster_metric_set(silhouette)
    Condition
      Error in `cluster_metric_set()`:
      ! The value "silhouette" is not a cluster metric. Did you mean `silhouette_avg`?

---

    Code
      cluster_metric_set(sse_within)
    Condition
      Error in `cluster_metric_set()`:
      ! `sse_within_total` is not a cluster metric. Did you mean `sse_within_total`?

