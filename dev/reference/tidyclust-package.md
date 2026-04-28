# tidyclust: A Tidy Interface to Clustering

The tidyclust package provides a tidy, unified interface to clustering
models, following the same design patterns as parsnip. It creates a
consistent API across different clustering functions and engines.

## Details

### Model specifications

- [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md):
  K-means clustering (stats, ClusterR, klaR, clustMixType engines)

- [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md):
  Hierarchical/agglomerative clustering (stats engine)

- [`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md):
  Density-based clustering (dbscan engine)

- [`gm_clust()`](https://tidyclust.tidymodels.org/dev/reference/gm_clust.md):
  Gaussian mixture model clustering (mclust engine)

### Key functions

- **Fitting**: [`fit()`](https://generics.r-lib.org/reference/fit.html),
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)

- **Prediction**:
  [`predict.cluster_fit()`](https://tidyclust.tidymodels.org/dev/reference/predict.cluster_fit.md)

- **Extraction**:
  [`extract_centroids()`](https://tidyclust.tidymodels.org/dev/reference/extract_centroids.md),
  [`extract_cluster_assignment()`](https://tidyclust.tidymodels.org/dev/reference/extract_cluster_assignment.md)

- **Metrics**:
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md),
  [`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md),
  [`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md)

- **Tuning**:
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)

### Getting started

    # Create a specification
    spec <- k_means(num_clusters = 3)

    # Fit to data
    fit <- fit(spec, ~., data = mtcars)

    # Extract results
    extract_centroids(fit)
    extract_cluster_assignment(fit)

## See also

- Package website: <https://tidyclust.tidymodels.org/>

- Bug reports: <https://github.com/tidymodels/tidyclust/issues>

## Author

**Maintainer**: Emil Hvitfeldt <emil.hvitfeldt@posit.co>
([ORCID](https://orcid.org/0000-0002-0679-1945))

Authors:

- Kelly Bodwin <kelly@bodwin.us>

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
