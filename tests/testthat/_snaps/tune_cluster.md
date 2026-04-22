# verbose argument works

    Code
      res <- tune_cluster(wflow, resamples = folds, grid = grid, control = control,
        metrics = metrics)
    Message
      i Fold1: preprocessor 1/3
      i Fold1: preprocessor 1/3 (prediction data)
      i Fold1: preprocessor 1/3, model 1/3
      i Fold1: preprocessor 1/3, model 1/3 (model metrics)
      i Fold1: preprocessor 1/3, model 1/3 (extracts)
      i Fold1: preprocessor 1/3, model 2/3
      i Fold1: preprocessor 1/3, model 2/3 (model metrics)
      i Fold1: preprocessor 1/3, model 2/3 (extracts)
      i Fold1: preprocessor 1/3, model 3/3
      i Fold1: preprocessor 1/3, model 3/3 (model metrics)
      i Fold1: preprocessor 1/3, model 3/3 (extracts)
      i Fold1: preprocessor 2/3
      i Fold1: preprocessor 2/3 (prediction data)
      i Fold1: preprocessor 2/3, model 1/3
      i Fold1: preprocessor 2/3, model 1/3 (model metrics)
      i Fold1: preprocessor 2/3, model 1/3 (extracts)
      i Fold1: preprocessor 2/3, model 2/3
      i Fold1: preprocessor 2/3, model 2/3 (model metrics)
      i Fold1: preprocessor 2/3, model 2/3 (extracts)
      i Fold1: preprocessor 2/3, model 3/3
      i Fold1: preprocessor 2/3, model 3/3 (model metrics)
      i Fold1: preprocessor 2/3, model 3/3 (extracts)
      i Fold1: preprocessor 3/3
      i Fold1: preprocessor 3/3 (prediction data)
      i Fold1: preprocessor 3/3, model 1/3
      i Fold1: preprocessor 3/3, model 1/3 (model metrics)
      i Fold1: preprocessor 3/3, model 1/3 (extracts)
      i Fold1: preprocessor 3/3, model 2/3
      i Fold1: preprocessor 3/3, model 2/3 (model metrics)
      i Fold1: preprocessor 3/3, model 2/3 (extracts)
      i Fold1: preprocessor 3/3, model 3/3
      i Fold1: preprocessor 3/3, model 3/3 (model metrics)
      i Fold1: preprocessor 3/3, model 3/3 (extracts)
      i Fold2: preprocessor 1/3
      i Fold2: preprocessor 1/3 (prediction data)
      i Fold2: preprocessor 1/3, model 1/3
      i Fold2: preprocessor 1/3, model 1/3 (model metrics)
      i Fold2: preprocessor 1/3, model 1/3 (extracts)
      i Fold2: preprocessor 1/3, model 2/3
      i Fold2: preprocessor 1/3, model 2/3 (model metrics)
      i Fold2: preprocessor 1/3, model 2/3 (extracts)
      i Fold2: preprocessor 1/3, model 3/3
      i Fold2: preprocessor 1/3, model 3/3 (model metrics)
      i Fold2: preprocessor 1/3, model 3/3 (extracts)
      i Fold2: preprocessor 2/3
      i Fold2: preprocessor 2/3 (prediction data)
      i Fold2: preprocessor 2/3, model 1/3
      i Fold2: preprocessor 2/3, model 1/3 (model metrics)
      i Fold2: preprocessor 2/3, model 1/3 (extracts)
      i Fold2: preprocessor 2/3, model 2/3
      i Fold2: preprocessor 2/3, model 2/3 (model metrics)
      i Fold2: preprocessor 2/3, model 2/3 (extracts)
      i Fold2: preprocessor 2/3, model 3/3
      i Fold2: preprocessor 2/3, model 3/3 (model metrics)
      i Fold2: preprocessor 2/3, model 3/3 (extracts)
      i Fold2: preprocessor 3/3
      i Fold2: preprocessor 3/3 (prediction data)
      i Fold2: preprocessor 3/3, model 1/3
      i Fold2: preprocessor 3/3, model 1/3 (model metrics)
      i Fold2: preprocessor 3/3, model 1/3 (extracts)
      i Fold2: preprocessor 3/3, model 2/3
      i Fold2: preprocessor 3/3, model 2/3 (model metrics)
      i Fold2: preprocessor 3/3, model 2/3 (extracts)
      i Fold2: preprocessor 3/3, model 3/3
      i Fold2: preprocessor 3/3, model 3/3 (model metrics)
      i Fold2: preprocessor 3/3, model 3/3 (extracts)

# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_cluster(helper_objects$kmeans_mod, ~z, resamples = data_folds,
      grid = cars_grid, control = tune::control_grid(extract = function(x) {
        1
      }, save_pred = TRUE))
    Message
      > A | error:   The following predictor was not found in `data`: "z".
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
      The `...` are not used in this function but 1 object was passed: "something"
    Output
      # Tuning results
      # 2-fold cross-validation 
      # A tibble: 2 x 4
        splits          id    .metrics         .notes          
        <list>          <chr> <list>           <list>          
      1 <split [16/16]> Fold1 <tibble [6 x 5]> <tibble [0 x 4]>
      2 <split [16/16]> Fold2 <tibble [6 x 5]> <tibble [0 x 4]>

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

# check_grid warns when no tuning parameters detected

    Code
      res <- tune_cluster(wflow, resamples = folds, grid = grid)
    Condition
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning.
      i Did you want to `tune()` parameters?

# check_grid errors when grid is not a data frame

    Code
      tune_cluster(wflow, resamples = folds, grid = "not a grid")
    Condition
      Error in `check_grid()`:
      ! `grid` should be a positive integer or a data frame.

# check_grid warns when duplicate rows in grid

    Code
      res <- tune_cluster(wflow, resamples = folds, grid = grid)
    Condition
      Warning:
      Duplicate rows in grid of tuning combinations found and removed.

# check_grid errors when grid has extra params

    Code
      tune_cluster(wflow, resamples = folds, grid = grid)
    Condition
      Error in `check_grid()`:
      ! The provided `grid` has parameter column 'extra_param' that has not been marked for tuning by `tune()`.

# check_grid errors when grid is missing params

    Code
      tune_cluster(wflow, resamples = folds, grid = grid)
    Condition
      Error in `check_grid()`:
      ! The provided `grid` is missing parameter column "'num_comp'" that has been marked for tuning by `tune()`.

# check_grid errors when numeric grid < 1

    Code
      tune_cluster(wflow, resamples = folds, grid = 0)
    Condition
      Error in `check_grid()`:
      ! `grid` should be a positive integer or a data frame.

# tune_cluster warns on apparent resamples

    Code
      tune_cluster(wflow, resamples = apparent_rs, grid = grid)
    Condition
      Warning:
      `tune_cluster()` was passed an `apparent()` resample.
      i Metrics from apparent resamples are excluded when summarizing with `collect_metrics(summarize = TRUE)` (the default). Use `collect_metrics(summarize = FALSE)` to see per-resample metrics.
    Output
      # Tuning results
      # Apparent sampling 
      # A tibble: 1 x 4
        splits          id       .metrics         .notes          
        <list>          <chr>    <list>           <list>          
      1 <split [32/32]> Apparent <tibble [4 x 5]> <tibble [0 x 4]>

