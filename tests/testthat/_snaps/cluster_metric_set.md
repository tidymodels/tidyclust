# cluster_metric_set works

    Code
      my_metrics(kmeans_fit)
    Error <rlang_error>
      In metric: `avg_silhouette`
      Must supply either a dataset or distance matrix to compute silhouettes.

# cluster_metric_set error with wrong input

    Code
      cluster_metric_set(mean)
    Error <rlang_error>
      
      The combination of metric functions must be:
      - only clustering metrics
      The following metric function types are being mixed:
      - other (mean <namespace:base>)

---

    Code
      cluster_metric_set(sse_ratio, mean)
    Error <rlang_error>
      
      The combination of metric functions must be:
      - only clustering metrics
      The following metric function types are being mixed:
      - cluster (sse_ratio)
      - other (mean <namespace:base>)

