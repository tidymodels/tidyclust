# verbose argument works

    Code
      res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control,
        metrics = metrics)
    Message
      i Fold1: preprocessor 1/3
      v Fold1: preprocessor 1/3
      i Fold1: preprocessor 1/3, model 1/3
      v Fold1: preprocessor 1/3, model 1/3
      i Fold1: preprocessor 1/3, model 1/3 (predictions)
      i Fold1: preprocessor 1/3, model 2/3
      v Fold1: preprocessor 1/3, model 2/3
      i Fold1: preprocessor 1/3, model 2/3 (predictions)
      i Fold1: preprocessor 1/3, model 3/3
      v Fold1: preprocessor 1/3, model 3/3
      i Fold1: preprocessor 1/3, model 3/3 (predictions)
      i Fold1: preprocessor 2/3
      v Fold1: preprocessor 2/3
      i Fold1: preprocessor 2/3, model 1/3
      v Fold1: preprocessor 2/3, model 1/3
      i Fold1: preprocessor 2/3, model 1/3 (predictions)
      i Fold1: preprocessor 2/3, model 2/3
      v Fold1: preprocessor 2/3, model 2/3
      i Fold1: preprocessor 2/3, model 2/3 (predictions)
      i Fold1: preprocessor 2/3, model 3/3
      v Fold1: preprocessor 2/3, model 3/3
      i Fold1: preprocessor 2/3, model 3/3 (predictions)
      i Fold1: preprocessor 3/3
      v Fold1: preprocessor 3/3
      i Fold1: preprocessor 3/3, model 1/3
      v Fold1: preprocessor 3/3, model 1/3
      i Fold1: preprocessor 3/3, model 1/3 (predictions)
      i Fold1: preprocessor 3/3, model 2/3
      v Fold1: preprocessor 3/3, model 2/3
      i Fold1: preprocessor 3/3, model 2/3 (predictions)
      i Fold1: preprocessor 3/3, model 3/3
      v Fold1: preprocessor 3/3, model 3/3
      i Fold1: preprocessor 3/3, model 3/3 (predictions)
      i Fold2: preprocessor 1/3
      v Fold2: preprocessor 1/3
      i Fold2: preprocessor 1/3, model 1/3
      v Fold2: preprocessor 1/3, model 1/3
      i Fold2: preprocessor 1/3, model 1/3 (predictions)
      i Fold2: preprocessor 1/3, model 2/3
      v Fold2: preprocessor 1/3, model 2/3
      i Fold2: preprocessor 1/3, model 2/3 (predictions)
      i Fold2: preprocessor 1/3, model 3/3
      v Fold2: preprocessor 1/3, model 3/3
      i Fold2: preprocessor 1/3, model 3/3 (predictions)
      i Fold2: preprocessor 2/3
      v Fold2: preprocessor 2/3
      i Fold2: preprocessor 2/3, model 1/3
      v Fold2: preprocessor 2/3, model 1/3
      i Fold2: preprocessor 2/3, model 1/3 (predictions)
      i Fold2: preprocessor 2/3, model 2/3
      v Fold2: preprocessor 2/3, model 2/3
      i Fold2: preprocessor 2/3, model 2/3 (predictions)
      i Fold2: preprocessor 2/3, model 3/3
      v Fold2: preprocessor 2/3, model 3/3
      i Fold2: preprocessor 2/3, model 3/3 (predictions)
      i Fold2: preprocessor 3/3
      v Fold2: preprocessor 3/3
      i Fold2: preprocessor 3/3, model 1/3
      v Fold2: preprocessor 3/3, model 1/3
      i Fold2: preprocessor 3/3, model 1/3 (predictions)
      i Fold2: preprocessor 3/3, model 2/3
      v Fold2: preprocessor 3/3, model 2/3
      i Fold2: preprocessor 3/3, model 2/3 (predictions)
      i Fold2: preprocessor 3/3, model 3/3
      v Fold2: preprocessor 3/3, model 3/3
      i Fold2: preprocessor 3/3, model 3/3 (predictions)

# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_cluster(helper_objects$kmeans_mod, ~z, resamples = data_folds,
      grid = cars_grid, control = tune::control_grid(extract = function(x) {
        1
      }, save_pred = TRUE))
    Message
      x Fold1: preprocessor 1/1: Error in `get_all_predictors()`:
      ! The following predi...
      x Fold2: preprocessor 1/1: Error in `get_all_predictors()`:
      ! The following predi...
    Condition
      Warning:
      All models failed.
      i See the `.notes` column.

# argument order gives errors for recipes

    Code
      tune_cluster(helper_objects$rec_tune_1, helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2))
    Condition
      Error in `tune_cluster()`:
      ! The first argument to `tune_cluster()` should be either a model or workflow.

# argument order gives errors for formula

    Code
      tune_cluster(mpg ~ ., helper_objects$kmeans_mod_no_tune, rsample::vfold_cv(
        mtcars, v = 2))
    Condition
      Error in `tune_cluster()`:
      ! The first argument to `tune_cluster()` should be either a model or workflow.

# ellipses with tune_cluster

    Code
      tune_cluster(wflow, resamples = folds, grid = 3, something = "wrong")
    Condition
      Warning:
      The `...` are not used in this function but one or more objects were passed: 'something'
    Output
      # Tuning results
      # 2-fold cross-validation 
      # A tibble: 2 x 4
        splits          id    .metrics         .notes          
        <list>          <chr> <list>           <list>          
      1 <split [16/16]> Fold1 <tibble [6 x 5]> <tibble [0 x 3]>
      2 <split [16/16]> Fold2 <tibble [6 x 5]> <tibble [0 x 3]>

# select_best() and show_best() works

    Code
      tmp <- tune::show_best(res)
    Condition
      Warning in `tune::show_best()`:
      No value of `metric` was given; "sse_within_total" will be used.

---

    Code
      tmp <- tune::select_best(res)
    Condition
      Warning in `tune::select_best()`:
      No value of `metric` was given; "sse_within_total" will be used.

