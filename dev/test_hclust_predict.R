library(tidyverse)
library(tidyclust)
#
# my_mod <- hier_clust(k = 3) |> fit(~., mtcars)
#
# #debugonce(tidyclust:::stats_hier_clust_predict)
# tidyclust:::stats_hier_clust_predict(my_mod, mtcars)
#
# my_mod <- hier_clust(k = 3, linkage_method = "single") |> fit(~., mtcars)
# my_mod$fit$method
# translate_tidyclust(hier_clust(k = 3, linkage_method = "single"))
#
# #debugonce(tidyclust:::stats_hier_clust_predict)
# tidyclust:::stats_hier_clust_predict(my_mod, mtcars)
#
# my_mod <- hier_clust(k = 3, linkage_method = "average") |> fit(~., mtcars)
#
# #debugonce(tidyclust:::stats_hier_clust_predict)
# tidyclust:::stats_hier_clust_predict(my_mod, mtcars)
#
# my_mod <- hier_clust(k = 3, linkage_method = "median") |> fit(~., mtcars)
#
# #debugonce(tidyclust:::stats_hier_clust_predict)
# tidyclust:::stats_hier_clust_predict(my_mod, mtcars)

my_mod <- hier_clust(k = 3, linkage_method = "centroid") |> fit(~., mtcars)

#debugonce(tidyclust:::stats_hier_clust_predict)
tidyclust:::stats_hier_clust_predict(my_mod, mtcars)

my_mod <- hier_clust(k = 3, linkage_method = "ward.D") |> fit(~., mtcars)
# debugonce(extract_fit_summary.hclust)
# extract_fit_summary.hclust(my_mod)

#debugonce(tidyclust:::stats_hier_clust_predict)
tidyclust:::stats_hier_clust_predict(my_mod$fit, mtcars)
predict(my_mod, mtcars)

silhouette_avg(my_mod)
