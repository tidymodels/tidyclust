# Simple Wrapper around hclust function

This wrapper prepares the data into a distance matrix to send to
[`stats::hclust`](https://rdrr.io/r/stats/hclust.html) and retains the
parameters `num_clusters` or `h` as an attribute.

## Usage

``` r
.hier_clust_fit_stats(
  x,
  num_clusters = NULL,
  cut_height = NULL,
  linkage_method = NULL,
  dist_fun = philentropy::distance
)
```

## Arguments

- x:

  matrix or data frame

- num_clusters:

  the number of clusters

- cut_height:

  the height to cut the dendrogram

- linkage_method:

  the agglomeration method to be used. This should be (an unambiguous
  abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
  `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA),
  `"median"` (= WPGMC) or `"centroid"` (= UPGMC).

- dist_fun:

  A distance function to use

## Value

A dendrogram
