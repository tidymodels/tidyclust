# cluster_metric_set() works

    Code
      my_metrics(kmeans_fit)
    Condition
      Error in `value[[3L]]()`:
      ! In metric: `silhouette_avg` Must supply either a dataset or distance matrix to compute silhouettes.

# cluster_metric_set() error with wrong input

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

# cluster_metric_set() errors with advice for some functions

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

# cluster_metric_set() errors when empty

    Code
      cluster_metric_set()
    Condition
      Error in `validate_not_empty()`:
      ! `cluster_metric_set()` requires at least 1 function supplied to `...`.

# cluster_metric_set() errors with non-functions

    Code
      cluster_metric_set("not_a_function")
    Condition
      Error in `validate_inputs_are_functions()`:
      ! All inputs to `cluster_metric_set()` must be functions.
      i These inputs are not: 1.

# print.cluster_metric_set() works

    Code
      print(metrics)
    Output
      # A tibble: 2 x 3
        metric    class          direction
        <chr>     <chr>          <chr>    
      1 sse_total cluster_metric zero     
      2 sse_ratio cluster_metric zero     

