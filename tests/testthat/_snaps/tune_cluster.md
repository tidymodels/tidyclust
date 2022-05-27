# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_cluster(helper_objects$kmeans_mod, y ~ z, resamples = data_folds,
      grid = cars_grid, control = tune::control_grid(extract = function(x) {
        1
      }, save_pred = TRUE))
    Warning <rlang_warning>
      All models failed. See the `.notes` column.

# argument order gives errors for recipes

    Code
      tune_cluster(helper_objects$rec_tune_1, helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2))
    Error <rlang_error>
      The first argument to [tune_cluster()] should be either a model or workflow.

# argument order gives errors for formula

    Code
      tune_cluster(mpg ~ ., helper_objects$kmeans_mod_no_tune, rsample::vfold_cv(
        mtcars, v = 2))
    Error <rlang_error>
      The first argument to [tune_cluster()] should be either a model or workflow.

# ellipses with tune_cluster

    Code
      tune_cluster(wflow, resamples = folds, grid = 3, something = "wrong")
    Warning <rlang_warning>
      The `...` are not used in this function but one or more objects were passed: 'something'
      All models failed. See the `.notes` column.
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 10 x 4
         splits         id     .metrics .notes
         <list>         <chr>  <list>   <list>
       1 <split [28/4]> Fold01 <NULL>   <NULL>
       2 <split [28/4]> Fold02 <NULL>   <NULL>
       3 <split [29/3]> Fold03 <NULL>   <NULL>
       4 <split [29/3]> Fold04 <NULL>   <NULL>
       5 <split [29/3]> Fold05 <NULL>   <NULL>
       6 <split [29/3]> Fold06 <NULL>   <NULL>
       7 <split [29/3]> Fold07 <NULL>   <NULL>
       8 <split [29/3]> Fold08 <NULL>   <NULL>
       9 <split [29/3]> Fold09 <NULL>   <NULL>
      10 <split [29/3]> Fold10 <NULL>   <NULL>

