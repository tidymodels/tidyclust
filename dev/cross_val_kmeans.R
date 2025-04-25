library(tidymodels)
library(tidyverse)
library(tidyclust)

## "Cross-validation" for kmeans

ir <- iris |> select(-Species)

cvs <- vfold_cv(ir, v = 5)

res <- data.frame(
  k = NA,
  i = NA,
  wss = NA,
  sil = NA,
  wss_2 = NA
)

for (k in 2:10) {
  km <- k_means(k = k) |>
    set_engine("stats")

  for (i in 1:5) {
    tmp_train <- training(cvs$splits[[i]])
    tmp_test <- testing(cvs$splits[[i]])

    km_fit <- km |> fit(~., data = tmp_train)

    wss <- km_fit |>
      sse_within_total_total(tmp_test)

    wss_2 <- km_fit$fit$tot.withinss

    sil <- km_fit |>
      silhouette_avg(tmp_test)

    res <- rbind(res, c(k = k, i = i, wss = wss, sil = sil, wss_2 = wss_2))
  }
}

res |>
  drop_na() |>
  ggplot(aes(x = factor(k), y = sil)) +
  geom_point()

### Second idea
## What if we cluster the whole data, then see how well subsamples are reclassified?
## This needs "predict"
## Doesn't really make sense yet

cvs <- vfold_cv(ir, v = 10)

res <- data.frame(
  k = NA,
  i = NA,
  acc = NA,
  f1 = NA
)

for (k in 2:10) {
  km <- k_means(k = k) |>
    set_engine("stats")

  full_fit <- km |> fit(~., data = ir)

  for (i in 1:10) {
    tmp_train <- training(cvs$splits[[i]])
    tmp_test <- testing(cvs$splits[[i]])

    km_fit <- km |> fit(~., data = tmp_train)

    dat <- tmp_test |>
      mutate(
        truth = predict(full_fit, tmp_test)$.pred_cluster,
        estimate = predict(km_fit, tmp_test)$.pred_cluster
      )

    thing <- reconcile_clusterings(dat$truth, dat$estimate)

    acc <- accuracy(thing, clusters_1, clusters_2)
    f1 <- f_meas(thing, clusters_1, clusters_2)

    res <- rbind(
      res,
      c(k = k, i = i, acc = acc$.estimate[1], f1 = f1$.estimate)
    )
  }
}

res |>
  ggplot(aes(x = factor(k), y = f1)) +
  geom_point()

### use orders from reconciling to order centers and check center similarity?
### or to get "raw probabilities" - what does that mean though?
### to do predict = raw
