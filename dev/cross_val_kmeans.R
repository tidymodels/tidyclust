library(tidymodels)
library(tidyverse)

## "Cross-validation" for kmeans

ir <- iris %>% select(-Species)

cvs <- vfold_cv(ir, v = 10)

res <- data.frame(
  k = NA,
  i = NA,
  wss = NA,
  sil = NA
)

for (k in 2:10) {

  km <- k_means(k = k) %>%
    set_engine_celery("stats")


  for (i in 1:10) {

    tmp_train <- training(cvs$splits[[i]])
    tmp_test <- testing(cvs$splits[[i]])

    km_fit <- km %>% fit(~., data = tmp_train)

    wss <- km_fit %>%
      tot_wss(tmp_test)

    sil <- km_fit %>%
      avg_silhouette(tmp_test)

    res <- rbind(res,
                 c(k = k, i = i, wss = wss, sil = sil))

  }

}

res %>%
  drop_na() %>%
  group_by(k) %>%
  summarize(wss = mean(wss)) %>%
  ggplot(aes(x = factor(k), y = wss)) +
  geom_point()


### not getting very consistent results here...
