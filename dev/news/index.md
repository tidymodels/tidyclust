# Changelog

## tidyclust (development version)

- [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  with the `stats` engine no longer errors when fit on sparse
  predictors, such as those produced by
  [`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html)
  in a workflow.
  ([\#263](https://github.com/tidymodels/tidyclust/issues/263))

## tidyclust 0.3.1

CRAN release: 2026-06-20

### Bug Fixes

- [`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md)
  now labels each metric in its output with the name supplied to it, so
  multiple metrics that wrap the same built-in metric (for example
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  with different `dist_fun` values) no longer collide and silently
  merge. ([\#257](https://github.com/tidymodels/tidyclust/issues/257))

- [`new_cluster_metric()`](https://tidyclust.tidymodels.org/dev/reference/new_cluster_metric.md)
  documentation now shows how to author a custom clustering metric, such
  as wrapping
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  with a non-default `dist_fun`, for use with
  [`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md).
  ([\#254](https://github.com/tidymodels/tidyclust/issues/254))

## tidyclust 0.3.0

CRAN release: 2026-05-21

### Deprecation

- [`finalize_model_tidyclust()`](https://tidyclust.tidymodels.org/dev/reference/finalize_model_tidyclust.md)
  and
  [`finalize_workflow_tidyclust()`](https://tidyclust.tidymodels.org/dev/reference/finalize_model_tidyclust.md)
  are deprecated. Use
  [`tune::finalize_model()`](https://tune.tidymodels.org/reference/finalize_model.html)
  and
  [`tune::finalize_workflow()`](https://tune.tidymodels.org/reference/finalize_model.html)
  instead, which now support `cluster_spec` objects natively.
  ([\#223](https://github.com/tidymodels/tidyclust/issues/223))

### New Models and Engines

- New
  [`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
  clustering specification for fitting DBSCAN models, with engines
  `"dbscan"` and `"hdbscan"`.
  ([\#209](https://github.com/tidymodels/tidyclust/issues/209),
  [\#238](https://github.com/tidymodels/tidyclust/issues/238))

- New
  [`gm_clust()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust.md)
  clustering specification for fitting Gaussian mixture models, with
  engine `"mclust"`.
  ([\#209](https://github.com/tidymodels/tidyclust/issues/209))

- New
  [`mean_shift()`](https://tidyclust.tidymodels.org/dev/reference/mean_shift.md)
  clustering specification for fitting mean shift models, which
  iteratively shift observations toward regions of high density and
  determine the number of clusters automatically. Engines `"LPCM"` and
  `"meanShiftR"` are supported.
  ([\#240](https://github.com/tidymodels/tidyclust/issues/240),
  [\#244](https://github.com/tidymodels/tidyclust/issues/244))

### Improvements

- Added `dials` parameter constructors
  [`radius()`](https://tidyclust.tidymodels.org/dev/reference/radius.md),
  [`min_points()`](https://tidyclust.tidymodels.org/dev/reference/min_points.md),
  [`circular()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust_params.md),
  [`zero_covariance()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust_params.md),
  [`shared_orientation()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust_params.md),
  [`shared_shape()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust_params.md),
  and
  [`shared_size()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust_params.md)
  so that tuning parameters for
  [`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
  and
  [`gm_clust()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust.md)
  resolve to real parameter objects rather than erroring on unexported
  `dials::` names.

- Added a “Getting started with tidyclust” vignette
  ([`vignette("tidyclust")`](https://tidyclust.tidymodels.org/dev/articles/tidyclust.md)).
  ([\#232](https://github.com/tidymodels/tidyclust/issues/232))

- Added `butcher` support for `cluster_fit` objects. `axe_data()`
  removes the training data stored in the fit, and `axe_env()` clears
  the environment reference from the preprocessing terms.
  ([\#126](https://github.com/tidymodels/tidyclust/issues/126))

- [`contr_one_hot()`](https://tidyclust.tidymodels.org/dev/reference/contr_one_hot.md)
  is now exported, fixing the `indicators = "one_hot"` code path in
  [`.convert_form_to_x_fit()`](https://tidyclust.tidymodels.org/dev/reference/convert_helpers.md)
  and
  [`.convert_form_to_x_new()`](https://tidyclust.tidymodels.org/dev/reference/convert_helpers.md).
  ([\#218](https://github.com/tidymodels/tidyclust/issues/218))

- [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md),
  [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md),
  and [`predict()`](https://rdrr.io/r/stats/predict.html) gain a
  `labels` argument, a character vector of cluster labels that overrides
  the auto-generated `prefix`-based labels.
  ([\#148](https://github.com/tidymodels/tidyclust/issues/148))

- [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
  gains a `dist_fun` argument for specifying a custom distance function.
  ([\#70](https://github.com/tidymodels/tidyclust/issues/70))

- [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
  documentation now clarifies that
  [`predict()`](https://rdrr.io/r/stats/predict.html) may not match
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  on training data: [`predict()`](https://rdrr.io/r/stats/predict.html)
  uses a distance-based heuristic while
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  uses [`cutree()`](https://rdrr.io/r/stats/cutree.html) based on the
  dendrogram structure.
  ([\#208](https://github.com/tidymodels/tidyclust/issues/208))

- The `dist_fun` argument accepted by cluster metrics is now documented,
  including how to use
  [philentropy](https://drostlab.github.io/philentropy/) to supply
  custom distance methods. See
  `vignette("tuning_and_metrics", package = "tidyclust")` for examples.
  ([\#185](https://github.com/tidymodels/tidyclust/issues/185))

- [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  now supports parallel processing via the `mirai` package in addition
  to `future`.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  now warns when passed an
  [`apparent()`](https://rsample.tidymodels.org/reference/apparent.html)
  resample. Metrics from apparent resamples are excluded by
  `collect_metrics(summarize = TRUE)` (the default) since tune 1.2.0,
  which caused unexpected `NA` values. Use
  `collect_metrics(summarize = FALSE)` to see per-resample metrics.
  ([\#193](https://github.com/tidymodels/tidyclust/issues/193))

- The `.notes` column returned by
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  now includes a `trace` column containing backtraces for errors and
  warnings, making it easier to debug failures.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

### Bug Fixes

- Fixed bug when trying to tune the `linkage_method` argument.
  ([\#206](https://github.com/tidymodels/tidyclust/issues/206),
  [@lgaborini](https://github.com/lgaborini))

- [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  now has `direction = "maximize"` instead of `direction = "zero"`, so
  that
  [`show_best()`](https://tune.tidymodels.org/reference/show_best.html)
  and
  [`select_best()`](https://tune.tidymodels.org/reference/show_best.html)
  correctly return models with the highest silhouette values.
  ([\#212](https://github.com/tidymodels/tidyclust/issues/212),
  [@dnldelarosa](https://github.com/dnldelarosa))

- [`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
  now correctly applies a custom `dist_fun` when `new_data` is `NULL` by
  using training data stored in the model.
  ([\#184](https://github.com/tidymodels/tidyclust/issues/184))

### Breaking Changes

- The `foreach` package is no longer supported for parallel processing
  in
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md).
  Use the `future` or `mirai` packages instead. See
  [`?tune::parallelism`](https://tune.tidymodels.org/reference/parallelism.html)
  for details.
  ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

- The `.config` column produced by
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  has changed from the `Preprocessor{num}_Model{num}` pattern to
  `pre{num}_mod{num}_post{num}` to align with updates in the tune
  package. ([\#220](https://github.com/tidymodels/tidyclust/issues/220))

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
