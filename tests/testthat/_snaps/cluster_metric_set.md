# cluster_metric_set works

    Code
      my_metrics(kmeans_fit)
    Condition
      Error in `value[[3L]]()`:
      ! In metric: `avg_silhouette`
      Must supply either a dataset or distance matrix to compute silhouettes.

# cluster_metric_set error with wrong input

    Code
      cluster_metric_set(mean)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only clustering metrics
      The following metric function types are being mixed:
      - other (mean <namespace:base>)

---

    Code
      cluster_metric_set(sse_ratio, mean)
    Condition
      Error in `validate_function_class()`:
      ! 
      The combination of metric functions must be:
      - only clustering metrics
      The following metric function types are being mixed:
      - cluster (sse_ratio)
      - other (mean <namespace:base>)

