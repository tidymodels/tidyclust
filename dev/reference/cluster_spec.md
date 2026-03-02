# Model Specification Information

An object with class "cluster_spec" is a container for information about
a model that will be fit.

## Details

The following model types are implemented in tidyclust:

- K-Means in
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)

- Hierarchical (Agglomerative) Clustering in
  [`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)

The main elements of the object are:

- `args`: A vector of the main arguments for the model. The names of
  these arguments may be different from their counterparts n the
  underlying model function. For example, for a
  [`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
  model, the argument name for the number of clusters are called
  "num_clusters" instead of "k" to make it more general and usable
  across different types of models (and to not be specific to a
  particular model function). The elements of `args` can
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html) with
  the use in
  [`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md).

For more information see <https://www.tidymodels.org/start/tuning/>. If
left to their defaults (`NULL`), the arguments will use the underlying
model functions default value. As discussed below, the arguments in
`args` are captured as quosures and are not immediately executed.

- `...`: Optional model-function-specific parameters. As with `args`,
  these will be quosures and can be
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

- `mode`: The type of model, such as "partition". Other modes will be
  added once the package adds more functionality.

- `method`: This is a slot that is filled in later by the model's
  constructor function. It generally contains lists of information that
  are used to create the fit and prediction code as well as required
  packages and similar data.

- `engine`: This character string declares exactly what software will be
  used. It can be a package name or a technology type.

This class and structure is the basis for how tidyclust stores model
objects prior to seeing the data.

## Argument Details

An important detail to understand when creating model specifications is
that they are intended to be functionally independent of the data. While
it is true that some tuning parameters are *data dependent*, the model
specification does not interact with the data at all.

For example, most R functions immediately evaluate their arguments. For
example, when calling `mean(dat_vec)`, the object `dat_vec` is
immediately evaluated inside of the function.

`tidyclust` model functions do not do this. For example, using

      k_means(num_clusters = ncol(mtcars) / 5)

**does not** execute `ncol(mtcars) / 5` when creating the specification.
This can be seen in the output:

      > k_means(num_clusters = ncol(mtcars) / 5)
      K Means Cluster Specification (partition)

      Main Arguments:
        num_clusters = ncol(mtcars)/5

      Computational engine: stats

The model functions save the argument *expressions* and their associated
environments (a.k.a. a quosure) to be evaluated later when either
[`fit.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
or
[`fit_xy.cluster_spec()`](https://tidyclust.tidymodels.org/dev/reference/fit.md)
are called with the actual data.

The consequence of this strategy is that any data required to get the
parameter values must be available when the model is fit. The two main
ways that this can fail is if:

1.  The data have been modified between the creation of the model
    specification and when the model fit function is invoked.

2.  If the model specification is saved and loaded into a new session
    where those same data objects do not exist.

The best way to avoid these issues is to not reference any data objects
in the global environment but to use data descriptors such as `.cols()`.
Another way of writing the previous specification is

      k_means(num_clusters = .cols() / 5)

This is not dependent on any specific data object and is evaluated
immediately before the model fitting process begins.

One less advantageous approach to solving this issue is to use
quasiquotation. This would insert the actual R object into the model
specification and might be the best idea when the data object is small.
For example, using

      k_means(num_clusters = ncol(!!mtcars) - 1)

would work (and be reproducible between sessions) but embeds the entire
mtcars data set into the `num_clusters` expression:

     > k_means(num_clusters = ncol(!!mtcars) / 5)
     K Means Cluster Specification (partition)

     Main Arguments:
       num_clusters = ncol(structure(list(mpg = c(21, 21, 22.8, 21.4, 18.7,<snip>

     Computational engine: stats

However, if there were an object with the number of columns in it, this
wouldn't be too bad:

     > num_clusters_val <- ncol(mtcars) / 5
     > num_clusters_val
     [1] 10
     > k_means(num_clusters = !!num_clusters_val)
     K Means Cluster Specification (partition)

     Main Arguments:
       num_clusters = 2.2

More information on quosures and quasiquotation can be found at
<https://adv-r.hadley.nz/quasiquotation.html>.
