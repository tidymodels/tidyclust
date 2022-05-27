library(tidymodels)
library(tidyverse)

## "Cross-validation" for kmeans

ir <- iris %>% select(-Species)

cvs <- vfold_cv(ir, v = 5)

res <- data.frame(
  k = NA,
  i = NA,
  wss = NA,
  sil = NA,
  wss_2 = NA
)

for (k in 2:10) {

  km <- k_means(k = k) %>%
    set_engine_celery("stats")


  for (i in 1:5) {

    tmp_train <- training(cvs$splits[[i]])
    tmp_test <- testing(cvs$splits[[i]])

    km_fit <- km %>% fit(~., data = tmp_train)

    wss <- km_fit %>%
      tot_wss(tmp_test)

    wss_2 <- km_fit$fit$tot.withinss

    sil <- km_fit %>%
      avg_silhouette(tmp_test)

    res <- rbind(res,
                 c(k = k, i = i, wss = wss, sil = sil, wss_2 = wss_2))

  }

}

res %>%
  drop_na() %>%
  ggplot(aes(x = factor(k), y = sil)) +
  geom_point()


### Second idea
## What if we cluster the whole data, then see how well subsamples are reclassified?
## This needs "predict"
## Doesn't really make sense yet

cvs <- vfold_cv(ir, v = 5)

res <- data.frame(
  k = NA,
  i = NA,
  acc = NA,
  roc_auc = NA
)

for (k in 2:10) {

  km <- k_means(k = k) %>%
    set_engine_celery("stats")

  full_fit <- km %>% fit(~., data = ir) %>%
    extract_cluster_assignment()


  for (i in 1:5) {

    tmp_train <- training(cvs$splits[[i]])
    tmp_test <- testing(cvs$splits[[i]])

    km_fit <- km %>% fit(~., data = tmp_train)

    dat <- tmp_train %>%
      mutate(
        truth = full_fit$.cluster[as.numeric(rownames(tmp_train))],
        estimate = extract_cluster_assignment(km_fit)$.cluster
      )

    acc <- accuracy(dat, truth, estimate)

    res <- rbind(res,
                 c(k = k, i = i, acc = acc$.estimate[1], roc_auc = NA))

  }

}


res %>%
  ggplot(aes(x = factor(k), y = acc)) +
  geom_boxplot()
