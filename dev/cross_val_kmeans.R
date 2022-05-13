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


