# verbose argument works

    Code
      res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control,
        metrics = metrics)
    Message
      i Fold01: preprocessor 1/3
      v Fold01: preprocessor 1/3
      i Fold01: preprocessor 1/3, model 1/3
      v Fold01: preprocessor 1/3, model 1/3
      i Fold01: preprocessor 1/3, model 1/3 (predictions)
      i Fold01: preprocessor 1/3, model 2/3
      v Fold01: preprocessor 1/3, model 2/3
      i Fold01: preprocessor 1/3, model 2/3 (predictions)
      i Fold01: preprocessor 1/3, model 3/3
      v Fold01: preprocessor 1/3, model 3/3
      i Fold01: preprocessor 1/3, model 3/3 (predictions)
      i Fold01: preprocessor 2/3
      v Fold01: preprocessor 2/3
      i Fold01: preprocessor 2/3, model 1/3
      v Fold01: preprocessor 2/3, model 1/3
      i Fold01: preprocessor 2/3, model 1/3 (predictions)
      i Fold01: preprocessor 2/3, model 2/3
      v Fold01: preprocessor 2/3, model 2/3
      i Fold01: preprocessor 2/3, model 2/3 (predictions)
      i Fold01: preprocessor 2/3, model 3/3
      v Fold01: preprocessor 2/3, model 3/3
      i Fold01: preprocessor 2/3, model 3/3 (predictions)
      i Fold01: preprocessor 3/3
      v Fold01: preprocessor 3/3
      i Fold01: preprocessor 3/3, model 1/3
      v Fold01: preprocessor 3/3, model 1/3
      i Fold01: preprocessor 3/3, model 1/3 (predictions)
      i Fold01: preprocessor 3/3, model 2/3
      v Fold01: preprocessor 3/3, model 2/3
      i Fold01: preprocessor 3/3, model 2/3 (predictions)
      i Fold01: preprocessor 3/3, model 3/3
      v Fold01: preprocessor 3/3, model 3/3
      i Fold01: preprocessor 3/3, model 3/3 (predictions)
      i Fold02: preprocessor 1/3
      v Fold02: preprocessor 1/3
      i Fold02: preprocessor 1/3, model 1/3
      v Fold02: preprocessor 1/3, model 1/3
      i Fold02: preprocessor 1/3, model 1/3 (predictions)
      i Fold02: preprocessor 1/3, model 2/3
      v Fold02: preprocessor 1/3, model 2/3
      i Fold02: preprocessor 1/3, model 2/3 (predictions)
      i Fold02: preprocessor 1/3, model 3/3
      v Fold02: preprocessor 1/3, model 3/3
      i Fold02: preprocessor 1/3, model 3/3 (predictions)
      i Fold02: preprocessor 2/3
      v Fold02: preprocessor 2/3
      i Fold02: preprocessor 2/3, model 1/3
      v Fold02: preprocessor 2/3, model 1/3
      i Fold02: preprocessor 2/3, model 1/3 (predictions)
      i Fold02: preprocessor 2/3, model 2/3
      v Fold02: preprocessor 2/3, model 2/3
      i Fold02: preprocessor 2/3, model 2/3 (predictions)
      i Fold02: preprocessor 2/3, model 3/3
      v Fold02: preprocessor 2/3, model 3/3
      i Fold02: preprocessor 2/3, model 3/3 (predictions)
      i Fold02: preprocessor 3/3
      v Fold02: preprocessor 3/3
      i Fold02: preprocessor 3/3, model 1/3
      v Fold02: preprocessor 3/3, model 1/3
      i Fold02: preprocessor 3/3, model 1/3 (predictions)
      i Fold02: preprocessor 3/3, model 2/3
      v Fold02: preprocessor 3/3, model 2/3
      i Fold02: preprocessor 3/3, model 2/3 (predictions)
      i Fold02: preprocessor 3/3, model 3/3
      v Fold02: preprocessor 3/3, model 3/3
      i Fold02: preprocessor 3/3, model 3/3 (predictions)
      i Fold03: preprocessor 1/3
      v Fold03: preprocessor 1/3
      i Fold03: preprocessor 1/3, model 1/3
      v Fold03: preprocessor 1/3, model 1/3
      i Fold03: preprocessor 1/3, model 1/3 (predictions)
      i Fold03: preprocessor 1/3, model 2/3
      v Fold03: preprocessor 1/3, model 2/3
      i Fold03: preprocessor 1/3, model 2/3 (predictions)
      i Fold03: preprocessor 1/3, model 3/3
      v Fold03: preprocessor 1/3, model 3/3
      i Fold03: preprocessor 1/3, model 3/3 (predictions)
      i Fold03: preprocessor 2/3
      v Fold03: preprocessor 2/3
      i Fold03: preprocessor 2/3, model 1/3
      v Fold03: preprocessor 2/3, model 1/3
      i Fold03: preprocessor 2/3, model 1/3 (predictions)
      i Fold03: preprocessor 2/3, model 2/3
      v Fold03: preprocessor 2/3, model 2/3
      i Fold03: preprocessor 2/3, model 2/3 (predictions)
      i Fold03: preprocessor 2/3, model 3/3
      v Fold03: preprocessor 2/3, model 3/3
      i Fold03: preprocessor 2/3, model 3/3 (predictions)
      i Fold03: preprocessor 3/3
      v Fold03: preprocessor 3/3
      i Fold03: preprocessor 3/3, model 1/3
      v Fold03: preprocessor 3/3, model 1/3
      i Fold03: preprocessor 3/3, model 1/3 (predictions)
      i Fold03: preprocessor 3/3, model 2/3
      v Fold03: preprocessor 3/3, model 2/3
      i Fold03: preprocessor 3/3, model 2/3 (predictions)
      i Fold03: preprocessor 3/3, model 3/3
      v Fold03: preprocessor 3/3, model 3/3
      i Fold03: preprocessor 3/3, model 3/3 (predictions)
      i Fold04: preprocessor 1/3
      v Fold04: preprocessor 1/3
      i Fold04: preprocessor 1/3, model 1/3
      v Fold04: preprocessor 1/3, model 1/3
      i Fold04: preprocessor 1/3, model 1/3 (predictions)
      i Fold04: preprocessor 1/3, model 2/3
      v Fold04: preprocessor 1/3, model 2/3
      i Fold04: preprocessor 1/3, model 2/3 (predictions)
      i Fold04: preprocessor 1/3, model 3/3
      v Fold04: preprocessor 1/3, model 3/3
      i Fold04: preprocessor 1/3, model 3/3 (predictions)
      i Fold04: preprocessor 2/3
      v Fold04: preprocessor 2/3
      i Fold04: preprocessor 2/3, model 1/3
      v Fold04: preprocessor 2/3, model 1/3
      i Fold04: preprocessor 2/3, model 1/3 (predictions)
      i Fold04: preprocessor 2/3, model 2/3
      v Fold04: preprocessor 2/3, model 2/3
      i Fold04: preprocessor 2/3, model 2/3 (predictions)
      i Fold04: preprocessor 2/3, model 3/3
      v Fold04: preprocessor 2/3, model 3/3
      i Fold04: preprocessor 2/3, model 3/3 (predictions)
      i Fold04: preprocessor 3/3
      v Fold04: preprocessor 3/3
      i Fold04: preprocessor 3/3, model 1/3
      v Fold04: preprocessor 3/3, model 1/3
      i Fold04: preprocessor 3/3, model 1/3 (predictions)
      i Fold04: preprocessor 3/3, model 2/3
      v Fold04: preprocessor 3/3, model 2/3
      i Fold04: preprocessor 3/3, model 2/3 (predictions)
      i Fold04: preprocessor 3/3, model 3/3
      v Fold04: preprocessor 3/3, model 3/3
      i Fold04: preprocessor 3/3, model 3/3 (predictions)
      i Fold05: preprocessor 1/3
      v Fold05: preprocessor 1/3
      i Fold05: preprocessor 1/3, model 1/3
      v Fold05: preprocessor 1/3, model 1/3
      i Fold05: preprocessor 1/3, model 1/3 (predictions)
      i Fold05: preprocessor 1/3, model 2/3
      v Fold05: preprocessor 1/3, model 2/3
      i Fold05: preprocessor 1/3, model 2/3 (predictions)
      i Fold05: preprocessor 1/3, model 3/3
      v Fold05: preprocessor 1/3, model 3/3
      i Fold05: preprocessor 1/3, model 3/3 (predictions)
      i Fold05: preprocessor 2/3
      v Fold05: preprocessor 2/3
      i Fold05: preprocessor 2/3, model 1/3
      v Fold05: preprocessor 2/3, model 1/3
      i Fold05: preprocessor 2/3, model 1/3 (predictions)
      i Fold05: preprocessor 2/3, model 2/3
      v Fold05: preprocessor 2/3, model 2/3
      i Fold05: preprocessor 2/3, model 2/3 (predictions)
      i Fold05: preprocessor 2/3, model 3/3
      v Fold05: preprocessor 2/3, model 3/3
      i Fold05: preprocessor 2/3, model 3/3 (predictions)
      i Fold05: preprocessor 3/3
      v Fold05: preprocessor 3/3
      i Fold05: preprocessor 3/3, model 1/3
      v Fold05: preprocessor 3/3, model 1/3
      i Fold05: preprocessor 3/3, model 1/3 (predictions)
      i Fold05: preprocessor 3/3, model 2/3
      v Fold05: preprocessor 3/3, model 2/3
      i Fold05: preprocessor 3/3, model 2/3 (predictions)
      i Fold05: preprocessor 3/3, model 3/3
      v Fold05: preprocessor 3/3, model 3/3
      i Fold05: preprocessor 3/3, model 3/3 (predictions)
      i Fold06: preprocessor 1/3
      v Fold06: preprocessor 1/3
      i Fold06: preprocessor 1/3, model 1/3
      v Fold06: preprocessor 1/3, model 1/3
      i Fold06: preprocessor 1/3, model 1/3 (predictions)
      i Fold06: preprocessor 1/3, model 2/3
      v Fold06: preprocessor 1/3, model 2/3
      i Fold06: preprocessor 1/3, model 2/3 (predictions)
      i Fold06: preprocessor 1/3, model 3/3
      v Fold06: preprocessor 1/3, model 3/3
      i Fold06: preprocessor 1/3, model 3/3 (predictions)
      i Fold06: preprocessor 2/3
      v Fold06: preprocessor 2/3
      i Fold06: preprocessor 2/3, model 1/3
      v Fold06: preprocessor 2/3, model 1/3
      i Fold06: preprocessor 2/3, model 1/3 (predictions)
      i Fold06: preprocessor 2/3, model 2/3
      v Fold06: preprocessor 2/3, model 2/3
      i Fold06: preprocessor 2/3, model 2/3 (predictions)
      i Fold06: preprocessor 2/3, model 3/3
      v Fold06: preprocessor 2/3, model 3/3
      i Fold06: preprocessor 2/3, model 3/3 (predictions)
      i Fold06: preprocessor 3/3
      v Fold06: preprocessor 3/3
      i Fold06: preprocessor 3/3, model 1/3
      v Fold06: preprocessor 3/3, model 1/3
      i Fold06: preprocessor 3/3, model 1/3 (predictions)
      i Fold06: preprocessor 3/3, model 2/3
      v Fold06: preprocessor 3/3, model 2/3
      i Fold06: preprocessor 3/3, model 2/3 (predictions)
      i Fold06: preprocessor 3/3, model 3/3
      v Fold06: preprocessor 3/3, model 3/3
      i Fold06: preprocessor 3/3, model 3/3 (predictions)
      i Fold07: preprocessor 1/3
      v Fold07: preprocessor 1/3
      i Fold07: preprocessor 1/3, model 1/3
      v Fold07: preprocessor 1/3, model 1/3
      i Fold07: preprocessor 1/3, model 1/3 (predictions)
      i Fold07: preprocessor 1/3, model 2/3
      v Fold07: preprocessor 1/3, model 2/3
      i Fold07: preprocessor 1/3, model 2/3 (predictions)
      i Fold07: preprocessor 1/3, model 3/3
      v Fold07: preprocessor 1/3, model 3/3
      i Fold07: preprocessor 1/3, model 3/3 (predictions)
      i Fold07: preprocessor 2/3
      v Fold07: preprocessor 2/3
      i Fold07: preprocessor 2/3, model 1/3
      v Fold07: preprocessor 2/3, model 1/3
      i Fold07: preprocessor 2/3, model 1/3 (predictions)
      i Fold07: preprocessor 2/3, model 2/3
      v Fold07: preprocessor 2/3, model 2/3
      i Fold07: preprocessor 2/3, model 2/3 (predictions)
      i Fold07: preprocessor 2/3, model 3/3
      v Fold07: preprocessor 2/3, model 3/3
      i Fold07: preprocessor 2/3, model 3/3 (predictions)
      i Fold07: preprocessor 3/3
      v Fold07: preprocessor 3/3
      i Fold07: preprocessor 3/3, model 1/3
      v Fold07: preprocessor 3/3, model 1/3
      i Fold07: preprocessor 3/3, model 1/3 (predictions)
      i Fold07: preprocessor 3/3, model 2/3
      v Fold07: preprocessor 3/3, model 2/3
      i Fold07: preprocessor 3/3, model 2/3 (predictions)
      i Fold07: preprocessor 3/3, model 3/3
      v Fold07: preprocessor 3/3, model 3/3
      i Fold07: preprocessor 3/3, model 3/3 (predictions)
      i Fold08: preprocessor 1/3
      v Fold08: preprocessor 1/3
      i Fold08: preprocessor 1/3, model 1/3
      v Fold08: preprocessor 1/3, model 1/3
      i Fold08: preprocessor 1/3, model 1/3 (predictions)
      i Fold08: preprocessor 1/3, model 2/3
      v Fold08: preprocessor 1/3, model 2/3
      i Fold08: preprocessor 1/3, model 2/3 (predictions)
      i Fold08: preprocessor 1/3, model 3/3
      v Fold08: preprocessor 1/3, model 3/3
      i Fold08: preprocessor 1/3, model 3/3 (predictions)
      i Fold08: preprocessor 2/3
      v Fold08: preprocessor 2/3
      i Fold08: preprocessor 2/3, model 1/3
      v Fold08: preprocessor 2/3, model 1/3
      i Fold08: preprocessor 2/3, model 1/3 (predictions)
      i Fold08: preprocessor 2/3, model 2/3
      v Fold08: preprocessor 2/3, model 2/3
      i Fold08: preprocessor 2/3, model 2/3 (predictions)
      i Fold08: preprocessor 2/3, model 3/3
      v Fold08: preprocessor 2/3, model 3/3
      i Fold08: preprocessor 2/3, model 3/3 (predictions)
      i Fold08: preprocessor 3/3
      v Fold08: preprocessor 3/3
      i Fold08: preprocessor 3/3, model 1/3
      v Fold08: preprocessor 3/3, model 1/3
      i Fold08: preprocessor 3/3, model 1/3 (predictions)
      i Fold08: preprocessor 3/3, model 2/3
      v Fold08: preprocessor 3/3, model 2/3
      i Fold08: preprocessor 3/3, model 2/3 (predictions)
      i Fold08: preprocessor 3/3, model 3/3
      v Fold08: preprocessor 3/3, model 3/3
      i Fold08: preprocessor 3/3, model 3/3 (predictions)
      i Fold09: preprocessor 1/3
      v Fold09: preprocessor 1/3
      i Fold09: preprocessor 1/3, model 1/3
      v Fold09: preprocessor 1/3, model 1/3
      i Fold09: preprocessor 1/3, model 1/3 (predictions)
      i Fold09: preprocessor 1/3, model 2/3
      v Fold09: preprocessor 1/3, model 2/3
      i Fold09: preprocessor 1/3, model 2/3 (predictions)
      i Fold09: preprocessor 1/3, model 3/3
      v Fold09: preprocessor 1/3, model 3/3
      i Fold09: preprocessor 1/3, model 3/3 (predictions)
      i Fold09: preprocessor 2/3
      v Fold09: preprocessor 2/3
      i Fold09: preprocessor 2/3, model 1/3
      v Fold09: preprocessor 2/3, model 1/3
      i Fold09: preprocessor 2/3, model 1/3 (predictions)
      i Fold09: preprocessor 2/3, model 2/3
      v Fold09: preprocessor 2/3, model 2/3
      i Fold09: preprocessor 2/3, model 2/3 (predictions)
      i Fold09: preprocessor 2/3, model 3/3
      v Fold09: preprocessor 2/3, model 3/3
      i Fold09: preprocessor 2/3, model 3/3 (predictions)
      i Fold09: preprocessor 3/3
      v Fold09: preprocessor 3/3
      i Fold09: preprocessor 3/3, model 1/3
      v Fold09: preprocessor 3/3, model 1/3
      i Fold09: preprocessor 3/3, model 1/3 (predictions)
      i Fold09: preprocessor 3/3, model 2/3
      v Fold09: preprocessor 3/3, model 2/3
      i Fold09: preprocessor 3/3, model 2/3 (predictions)
      i Fold09: preprocessor 3/3, model 3/3
      v Fold09: preprocessor 3/3, model 3/3
      i Fold09: preprocessor 3/3, model 3/3 (predictions)
      i Fold10: preprocessor 1/3
      v Fold10: preprocessor 1/3
      i Fold10: preprocessor 1/3, model 1/3
      v Fold10: preprocessor 1/3, model 1/3
      i Fold10: preprocessor 1/3, model 1/3 (predictions)
      i Fold10: preprocessor 1/3, model 2/3
      v Fold10: preprocessor 1/3, model 2/3
      i Fold10: preprocessor 1/3, model 2/3 (predictions)
      i Fold10: preprocessor 1/3, model 3/3
      v Fold10: preprocessor 1/3, model 3/3
      i Fold10: preprocessor 1/3, model 3/3 (predictions)
      i Fold10: preprocessor 2/3
      v Fold10: preprocessor 2/3
      i Fold10: preprocessor 2/3, model 1/3
      v Fold10: preprocessor 2/3, model 1/3
      i Fold10: preprocessor 2/3, model 1/3 (predictions)
      i Fold10: preprocessor 2/3, model 2/3
      v Fold10: preprocessor 2/3, model 2/3
      i Fold10: preprocessor 2/3, model 2/3 (predictions)
      i Fold10: preprocessor 2/3, model 3/3
      v Fold10: preprocessor 2/3, model 3/3
      i Fold10: preprocessor 2/3, model 3/3 (predictions)
      i Fold10: preprocessor 3/3
      v Fold10: preprocessor 3/3
      i Fold10: preprocessor 3/3, model 1/3
      v Fold10: preprocessor 3/3, model 1/3
      i Fold10: preprocessor 3/3, model 1/3 (predictions)
      i Fold10: preprocessor 3/3, model 2/3
      v Fold10: preprocessor 3/3, model 2/3
      i Fold10: preprocessor 3/3, model 2/3 (predictions)
      i Fold10: preprocessor 3/3, model 3/3
      v Fold10: preprocessor 3/3, model 3/3
      i Fold10: preprocessor 3/3, model 3/3 (predictions)

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
      All models failed. See the `.notes` column.

# argument order gives errors for recipes

    Code
      tune_cluster(helper_objects$rec_tune_1, helper_objects$kmeans_mod_no_tune,
      rsample::vfold_cv(mtcars, v = 2))
    Condition
      Error in `tune_cluster()`:
      ! The first argument to [tune_cluster()] should be either a model or workflow.

# argument order gives errors for formula

    Code
      tune_cluster(mpg ~ ., helper_objects$kmeans_mod_no_tune, rsample::vfold_cv(
        mtcars, v = 2))
    Condition
      Error in `tune_cluster()`:
      ! The first argument to [tune_cluster()] should be either a model or workflow.

# ellipses with tune_cluster

    Code
      tune_cluster(wflow, resamples = folds, grid = 3, something = "wrong")
    Condition
      Warning:
      The `...` are not used in this function but one or more objects were passed: 'something'
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 10 x 4
         splits         id     .metrics         .notes          
         <list>         <chr>  <list>           <list>          
       1 <split [28/4]> Fold01 <tibble [4 x 5]> <tibble [0 x 3]>
       2 <split [28/4]> Fold02 <tibble [4 x 5]> <tibble [0 x 3]>
       3 <split [29/3]> Fold03 <tibble [4 x 5]> <tibble [0 x 3]>
       4 <split [29/3]> Fold04 <tibble [4 x 5]> <tibble [0 x 3]>
       5 <split [29/3]> Fold05 <tibble [4 x 5]> <tibble [0 x 3]>
       6 <split [29/3]> Fold06 <tibble [4 x 5]> <tibble [0 x 3]>
       7 <split [29/3]> Fold07 <tibble [4 x 5]> <tibble [0 x 3]>
       8 <split [29/3]> Fold08 <tibble [4 x 5]> <tibble [0 x 3]>
       9 <split [29/3]> Fold09 <tibble [4 x 5]> <tibble [0 x 3]>
      10 <split [29/3]> Fold10 <tibble [4 x 5]> <tibble [0 x 3]>

# select_best() and show_best() works

    Code
      tmp <- tune::show_best(res)
    Condition
      Warning:
      No value of `metric` was given; metric 'sse_within' will be used.

---

    Code
      tmp <- tune::select_best(res)
    Condition
      Warning:
      No value of `metric` was given; metric 'sse_within' will be used.

