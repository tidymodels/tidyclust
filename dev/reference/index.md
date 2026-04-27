# Package index

## Specifications

These cluster specification functions are used to specify the type of
model you want to do. These functions work in a similar fashion to the
[model specification function from
parsnip](https://parsnip.tidymodels.org/reference/index.html#models).

- [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  : K-Means
- [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
  : Hierarchical (Agglomerative) Clustering
- [`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
  : Density-Based Spatial Clustering of Applications with Noise (DBSCAN)
- [`gm_clust()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust.md)
  : Gaussian Mixture Models (GMM)
- [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  : Model Specification Information
- [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
  : Model Fit Object Information

## Fit and Inspect

These functions are the generics that are supported for specifications
created with tidyclust.

- [`fit(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
  [`fit_xy(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
  : Fit a Model Specification to a Data Set
- [`set_args(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/set_args.cluster_spec.md)
  : Change arguments of a cluster specification
- [`set_engine(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/set_engine.cluster_spec.md)
  : Change engine of a cluster specification
- [`set_mode(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/set_mode.cluster_spec.md)
  : Change mode of a cluster specification
- [`augment(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/augment.md)
  : Augment data with predictions
- [`glance(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/glance.cluster_fit.md)
  : Construct a single row summary "glance" of a model, fit, or other
  object
- [`tidy(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/tidy.cluster_fit.md)
  : Turn a tidyclust model object into a tidy tibble
- [`extract_fit_engine(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/extract-tidyclust.md)
  [`extract_parameter_set_dials(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/extract-tidyclust.md)
  : Extract elements of a tidyclust model object

## Prediction

Once the cluster specification have been fit, you are likely to want to
look at where the clusters are and which observations are associated
with which cluster.

- [`predict(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)
  [`predict_raw(`*`<cluster_fit>`*`)`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)
  : Model predictions
- [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)
  : Extract cluster assignments from model
- [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md)
  : Extract clusters from model

## Model based performance metrics

These metrics use the fitted clustering model to extract values denoting
how well the model works.

- [`cluster_metric_set()`](https://tidyclust.tidymodels.org/dev/reference/cluster_metric_set.md)
  : Combine metric functions
- [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  [`silhouette_avg_vec()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
  : Measures average silhouette across all observations
- [`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md)
  [`sse_ratio_vec()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md)
  : Compute the ratio of the WSS to the total SSE
- [`sse_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md)
  [`sse_total_vec()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md)
  : Compute the total sum of squares
- [`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
  [`sse_within_total_vec()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
  : Compute the sum of within-cluster SSE
- [`silhouette()`](https://tidyclust.tidymodels.org/dev/reference/silhouette.md)
  : Measures silhouette between clusters
- [`sse_within()`](https://tidyclust.tidymodels.org/dev/reference/sse_within.md)
  : Calculates Sum of Squared Error in each cluster

## Tuning

Functions to allow multiple cluster specifications to be fit at once.

- [`control_cluster()`](https://tidyclust.tidymodels.org/dev/reference/control_cluster.md)
  : Control the fit function
- [`update(`*`<db_clust>`*`)`](https://tidyclust.tidymodels.org/dev/reference/tidyclust_update.md)
  [`update(`*`<gm_clust>`*`)`](https://tidyclust.tidymodels.org/dev/reference/tidyclust_update.md)
  [`update(`*`<hier_clust>`*`)`](https://tidyclust.tidymodels.org/dev/reference/tidyclust_update.md)
  [`update(`*`<k_means>`*`)`](https://tidyclust.tidymodels.org/dev/reference/tidyclust_update.md)
  : Update a cluster specification
- [`finalize_model_tidyclust()`](https://tidyclust.tidymodels.org/dev/reference/finalize_model_tidyclust.md)
  [`finalize_workflow_tidyclust()`](https://tidyclust.tidymodels.org/dev/reference/finalize_model_tidyclust.md)
  **\[deprecated\]** : Splice final parameters into objects
- [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
  : Model tuning via grid search

## Tuning Objects

Dials objects.

- [`cut_height()`](https://tidyclust.tidymodels.org/dev/reference/cut_height.md)
  : Cut Height
- [`linkage_method()`](https://tidyclust.tidymodels.org/dev/reference/linkage_method.md)
  [`values_linkage_method`](https://tidyclust.tidymodels.org/dev/reference/linkage_method.md)
  : The agglomeration Linkage method

## Developer tools

- [`contr_one_hot()`](https://tidyclust.tidymodels.org/dev/reference/contr_one_hot.md)
  : One-hot contrast matrix
- [`extract_fit_summary()`](https://tidyclust.tidymodels.org/dev/reference/extract_fit_summary.md)
  : S3 method to get fitted model summary info depending on engine
- [`get_centroid_dists()`](https://tidyclust.tidymodels.org/dev/reference/get_centroid_dists.md)
  : Computes distance from observations to centroids
- [`new_cluster_metric()`](https://tidyclust.tidymodels.org/dev/reference/new_cluster_metric.md)
  : Construct a new clustering metric function
- [`prep_data_dist()`](https://tidyclust.tidymodels.org/dev/reference/prep_data_dist.md)
  : Prepares data and distance matrices for metric calculation
- [`reconcile_clusterings_mapping()`](https://tidyclust.tidymodels.org/dev/reference/reconcile_clusterings_mapping.md)
  : Relabels clusters to match another cluster assignment
- [`translate_tidyclust()`](https://tidyclust.tidymodels.org/dev/reference/translate_tidyclust.md)
  : Resolve a Model Specification for a Computational Engine
- [`min_grid(`*`<cluster_spec>`*`)`](https://tidyclust.tidymodels.org/dev/reference/min_grid.cluster_spec.md)
  : Determine the minimum set of model fits
