# Tuning Cluster Models

## Setup

Load libraries:

``` r
library(tidyclust)
library(tidyverse)
library(tidymodels)
set.seed(838383)
```

Load and clean a dataset:

``` r
data("penguins", package = "modeldata")

penguins <- penguins |>
  drop_na()
```

## Tuning in unsupervised settings

In *supervised modeling* scenarios, we observe values of a target (or
“response”) variable, and we measure the success of our model based on
how well it predicts future response values. To select hyperparameter
values, we **tune** them, trying many possible values and measuring how
well each performs when predicting target values of test data.

In the *unsupervised modeling* setting of `tidyclust`, there is no such
objective measure of success. Clustering analyses are typically
exploratory rather than testable. Nonetheless, the core tuning principle
of varying inputs and quantifying results is still applicable.

## Specify and fit a model

In this example, we will fit a $k$-means cluster model to the
`palmerpenguins` dataset, using only the bill length and bill depth of
penguins as predictors.

(Please refer to the k-means vignette for an in-depth discussion of this
model specification.)

Our goal will be to select an appropriate number of clusters for the
model based on metrics.

First, we set up cross-validation samples for our data:

``` r
penguins_cv <- vfold_cv(penguins, v = 5)
```

Next, we specify our model with a tuning parameter, make a workflow, and
establish a range of possible values of `num_clusters` to try:

``` r
kmeans_spec <- k_means(num_clusters = tune())

penguins_rec <- recipe(~ bill_length_mm + bill_depth_mm,
  data = penguins
)

kmeans_wflow <- workflow(penguins_rec, kmeans_spec)

clust_num_grid <- grid_regular(num_clusters(),
  levels = 10
)

clust_num_grid
#> # A tibble: 10 × 1
#>    num_clusters
#>           <int>
#>  1            1
#>  2            2
#>  3            3
#>  4            4
#>  5            5
#>  6            6
#>  7            7
#>  8            8
#>  9            9
#> 10           10
```

Then, we can use
[`tune_cluster()`](https://tidyclust.tidymodels.org/dev/reference/tune_cluster.md)
to compute metrics on each cross-validation split, for each possible
choice of number of clusters.

``` r
res <- tune_cluster(
  kmeans_wflow,
  resamples = penguins_cv,
  grid = clust_num_grid,
  control = control_grid(save_pred = TRUE, extract = identity),
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)

res
#> # Tuning results
#> # 5-fold cross-validation 
#> # A tibble: 5 × 6
#>   splits           id    .metrics .notes   .extracts .predictions      
#>   <list>           <chr> <list>   <list>   <list>    <list>            
#> 1 <split [266/67]> Fold1 <tibble> <tibble> <tibble>  <tibble [670 × 4]>
#> 2 <split [266/67]> Fold2 <tibble> <tibble> <tibble>  <tibble [670 × 4]>
#> 3 <split [266/67]> Fold3 <tibble> <tibble> <tibble>  <tibble [670 × 4]>
#> 4 <split [267/66]> Fold4 <tibble> <tibble> <tibble>  <tibble [660 × 4]>
#> 5 <split [267/66]> Fold5 <tibble> <tibble> <tibble>  <tibble [660 × 4]>
```

``` r
res_metrics <- res |> collect_metrics()
res_metrics
#> # A tibble: 30 × 7
#>    num_clusters .metric        .estimator    mean     n std_err .config
#>           <int> <chr>          <chr>        <dbl> <int>   <dbl> <chr>  
#>  1            1 sse_ratio      standard   1   e+0     5 0       Prepro…
#>  2            1 sse_total      standard   8.97e+3     5 1.19e+2 Prepro…
#>  3            1 sse_within_to… standard   8.97e+3     5 1.19e+2 Prepro…
#>  4            2 sse_ratio      standard   3.21e-1     5 1.41e-3 Prepro…
#>  5            2 sse_total      standard   8.97e+3     5 1.19e+2 Prepro…
#>  6            2 sse_within_to… standard   2.88e+3     5 4.73e+1 Prepro…
#>  7            3 sse_ratio      standard   2.02e-1     5 2.21e-3 Prepro…
#>  8            3 sse_total      standard   8.97e+3     5 1.19e+2 Prepro…
#>  9            3 sse_within_to… standard   1.81e+3     5 3.47e+1 Prepro…
#> 10            4 sse_ratio      standard   1.60e-1     5 5.79e-3 Prepro…
#> # ℹ 20 more rows
```

### Choosing hyperparameters

In supervised learning, we would choose the model with the best value of
a target metric. However, clustering models in general have no such
local maxima or minima. With more clusters in the model, we would always
expect the within sum-of-squares to be smaller.

A common approach to choosing a number of clusters is to look for an
“elbow”, or notable bend, in the plot of WSS/TSS ratio by cluster
number:

``` r
res_metrics |>
  filter(.metric == "sse_ratio") |>
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  ylab("mean WSS/TSS ratio, over 5 folds") +
  xlab("Number of clusters") +
  scale_x_continuous(breaks = 1:10)
```

![Connected line chart. Number of clusters along the x-axis, mean
WSS/TSS ratio, over 5 folds along the y-axis. X-axis ranges from 1 to
10, with 1 having an value of 1, 2 having a value of 0.35, and the rest
haver an ever decreasing
value.](tuning_and_metrics_files/figure-html/unnamed-chunk-7-1.png)

At each increase in the number of clusters, the WSS/TSS ratio decreases,
with the amount of decrease getting smaller as the number of clusters
grows. We might argue that the drop from two clusters to three, or from
three to four, is a bit more extreme than the subsequent drops, so we
should probably choose three or four clusters.
