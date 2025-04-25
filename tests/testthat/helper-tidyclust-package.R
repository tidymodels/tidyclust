new_rng_snapshots <- utils::compareVersion(
  "3.6.0",
  as.character(getRversion())
) >
  0

helper_objects_tidyclust <- function() {
  rec_tune_1 <-
    recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())

  rec_no_tune_1 <-
    recipes::recipe(~., data = mtcars) |>
    recipes::step_normalize(recipes::all_predictors())

  kmeans_mod_no_tune <- k_means(num_clusters = 2)

  kmeans_mod <- k_means(num_clusters = tune())

  list(
    rec_tune_1 = rec_tune_1,
    rec_no_tune_1 = rec_no_tune_1,
    kmeans_mod = kmeans_mod,
    kmeans_mod_no_tune = kmeans_mod_no_tune
  )
}

new_empty_quosure <- function(expr) {
  rlang::new_quosure(expr, env = rlang::empty_env())
}
