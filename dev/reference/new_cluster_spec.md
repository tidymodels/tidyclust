# Functions required for tidyclust-adjacent packages

These functions are helpful when creating new packages that will
register new cluster specifications.

## Usage

``` r
new_cluster_spec(cls, args, eng_args, mode, method, engine)
```

## Arguments

- cls:

  A single character string for the model type (e.g. `"k_means"`).

- args:

  A named list of main model arguments.

- eng_args:

  A named list of engine-specific arguments.

- mode:

  A single character string for the model mode (e.g. `"partition"`).

- method:

  A list of method details or `NULL`.

- engine:

  A single character string for the computational engine.

## Value

A
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
object made to work with tidyclust.
