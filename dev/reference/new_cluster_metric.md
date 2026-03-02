# Construct a new clustering metric function

These functions provide convenient wrappers to create the one type of
metric functions in celrry: clustering metrics. They add a
metric-specific class to `fn`. These features are used by
[`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md)
and by
[`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
when tuning.

## Usage

``` r
new_cluster_metric(fn, direction)
```

## Arguments

- fn:

  A function.

- direction:

  A string. One of:

  - `"maximize"`

  - `"minimize"`

  - `"zero"`

## Value

A `cluster_metric` object.
