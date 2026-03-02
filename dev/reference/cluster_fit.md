# Model Fit Object Information

An object with class "cluster_fit" is a container for information about
a model that has been fit to the data.

## Details

The following model types are implemented in tidyclust:

- K-Means in
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)

- Hierarchical (Agglomerative) Clustering in
  [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)

The main elements of the object are:

- `spec`: A
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object.

- `fit`: The object produced by the fitting function.

- `preproc`: This contains any data-specific information required to
  process new a sample point for prediction. For example, if the
  underlying model function requires arguments `x` and the user passed a
  formula to `fit`, the `preproc` object would contain items such as the
  terms object and so on. When no information is required, this is `NA`.

As discussed in the documentation for
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md),
the original arguments to the specification are saved as quosures. These
are evaluated for the `cluster_fit` object prior to fitting. If the
resulting model object prints its call, any user-defined options are
shown in the call preceded by a tilde (see the example below). This is a
result of the use of quosures in the specification.

This class and structure is the basis for how tidyclust stores model
objects after seeing the data and applying a model.
