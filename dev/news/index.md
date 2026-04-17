# Changelog

## tidyclust (development version)

### New Clustering Specifications

- The
  [`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
  clustering specification has been added. This specification allows for
  the use of the DBSCAN algorithm using the dbscan engine.
  ([\#209](https://github.com/tidymodels/tidyclust/issues/209))

- The
  [`gm_clust()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust.md)
  clustering specification has been added. This specification allows for
  the fitting of Gaussian mixture models using the mclust engine.
  ([\#209](https://github.com/tidymodels/tidyclust/issues/209))

- The `.config` column produced by
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  has changed from the `Preprocessor{num}_Model{num}` pattern to
  `pre{num}_mod{num}_post{num}` to align with updates in the tune
  package. ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- The `foreach` package is no longer supported for parallel processing
  in
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md).
  Use the `future` or `mirai` packages instead. See
  [`?tune::parallelism`](https://tune.tidymodels.org/reference/parallelism.html)
  for details.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  now supports parallel processing via the `mirai` package in addition
  to `future`.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- The `.notes` column returned by
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  now includes a `trace` column containing backtraces for errors and
  warnings, making it easier to debug failures.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- Fixed bug when trying to tune the `linkage_method` argument.
  ([\#206](https://github.com/tidymodels/tidyclust/issues/206),
  [@lgaborini](https://github.com/lgaborini))

## tidyclust 0.2.4

CRAN release: 2025-01-27

- The philentropy package is now used to calculate distances rather than
  Rfast. ([\#199](https://github.com/tidymodels/tidyclust/issues/199))

## tidyclust 0.2.3

CRAN release: 2024-07-02

- Update to fix revdep issue for clustMixType.
  ([\#190](https://github.com/tidymodels/tidyclust/issues/190))

## tidyclust 0.2.2

CRAN release: 2024-06-17

- Update to fix revdep issue for ClusterR.
  ([\#186](https://github.com/tidymodels/tidyclust/issues/186))

## tidyclust 0.2.1

CRAN release: 2024-02-28

- Small change to let tune package have easy CRAN release.
  ([\#178](https://github.com/tidymodels/tidyclust/issues/178))

## tidyclust 0.2.0

CRAN release: 2023-09-25

### New Engines

- The clustMixType engine as been added to
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md).
  This engine allows fitting of k-prototype models.
  ([\#63](https://github.com/tidymodels/tidyclust/issues/63))

- The klaR engine as been added to
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md).
  This engine allows fitting of k-modes models.
  ([\#63](https://github.com/tidymodels/tidyclust/issues/63))

### Improvements

- Engine specific documentation has been added for all models and
  engines. ([\#159](https://github.com/tidymodels/tidyclust/issues/159))

### Bug Fixes

- Fixed bug where engine specific arguments were passed along for
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  when the engine ClusterR.
  ([\#142](https://github.com/tidymodels/tidyclust/issues/142))

- Fixed bug where `prefix` argument wouldn’t be correctly passed through
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md),
  [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md),
  and [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#145](https://github.com/tidymodels/tidyclust/issues/145))

- Metric functions now error informatively if used with unfit cluster
  specifications.
  ([\#146](https://github.com/tidymodels/tidyclust/issues/146))

- Fixed bug that caused cluster ordering in extract_fit_summary().
  ([\#136](https://github.com/tidymodels/tidyclust/issues/136))

- Using
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md),
  [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
  and [`predict()`](https://rdrr.io/r/stats/predict.html) on a fitted
  [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
  model without specifying `num_clust` or `cut_height` now gives more
  informative error message.
  ([\#147](https://github.com/tidymodels/tidyclust/issues/147))

- [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  now errors informatively if
  [`fit()`](https://generics.r-lib.org/reference/fit.html) without
  `num_clust` specified.
  ([\#134](https://github.com/tidymodels/tidyclust/issues/134))

- Fixed bug where levels didn’t match number of clusters if prediction
  on fewer number of observations.
  ([\#158](https://github.com/tidymodels/tidyclust/issues/158))

- Fixed bug where
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  would error if used with an recipe that contained non-predictor
  variables such as id variables.
  ([\#124](https://github.com/tidymodels/tidyclust/issues/124))

### Breaking Changes

- Exported internal functions `ClusterR_kmeans_fit()`,
  `stats_kmeans_fit()`, and `hclust_fit()` have been renamed to
  [`.k_means_fit_ClusterR()`](https://tidyclust.tidymodels.org/dev/reference/dot-k_means_fit_ClusterR.md),
  [`.k_means_fit_stats()`](https://tidyclust.tidymodels.org/dev/reference/dot-k_means_fit_stats.md),
  and
  [`.hier_clust_fit_stats()`](https://tidyclust.tidymodels.org/dev/reference/dot-hier_clust_fit_stats.md)
  to reduce visibility for users.

- Cluster reordering is now done at the fitting time, not the extraction
  and prediction time.
  ([\#154](https://github.com/tidymodels/tidyclust/issues/154))

## tidyclust 0.1.2

CRAN release: 2023-02-23

- The cluster specification methods for
  [`generics::tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
  and
  [`generics::tunable()`](https://generics.r-lib.org/reference/tunable.html)
  are now registered unconditionally
  ([\#115](https://github.com/tidymodels/tidyclust/issues/115)).

## tidyclust 0.1.1

CRAN release: 2022-12-20

- Fixed bug where
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  and [`predict()`](https://rdrr.io/r/stats/predict.html) sometimes
  didn’t have agreement of clusters.
  ([\#94](https://github.com/tidymodels/tidyclust/issues/94))

- [`silhouette()`](https://tidyclust.tidymodels.org/dev/reference/silhouette.md)
  and
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  now return NAs instead of erroring when applied to a clustering object
  with 1 cluster.
  ([\#104](https://github.com/tidymodels/tidyclust/issues/104))

- Fixed bug where
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  doesn’t work for
  [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
  models in workflows where `num_clusters` is specified in
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md).

## tidyclust 0.1.0

CRAN release: 2022-11-24

- Added a `NEWS.md` file to track changes to the package.
