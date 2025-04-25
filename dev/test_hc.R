library(tidyverse)
library(celery)

ir <- iris[, -5]

hclust(dist(ir))

bob <- hclust_fit(ir)

hc <- hier_clust(k = 3) |>
  fit(~., data = ir)

km <- k_means(k = 3) |>
  fit(~., data = ir)

thing <- tibble(
  km_c = extract_cluster_assignment(km)$.cluster,
  hc_c = extract_cluster_assignment(hc)$.cluster,
  truth = iris$Species
)

thing |>
  count(hc_c, truth)

cutree(hc$fit, k = 3)

# hc |>
#   extract_fit_engine() |>
#   cutree(k = 3)

## reconcile?
