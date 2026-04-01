# errors in parallel workers are caught with future

    Code
      res <- tune_cluster(helper_objects$kmeans_mod, ~z, resamples = folds, grid = grid,
      control = tune::control_grid(save_pred = TRUE))
    Condition
      Warning in `serializedSize()`:
      'package:tidyclust' may not be available when loading
      Warning in `serializedSize()`:
      'package:tidyclust' may not be available when loading
    Message
      → A | error:   The following predictor was not found in `data`: "z".
      → A | error:   The following predictor was not found in `data`: "z".
    Condition
      Warning:
      All models failed.
      i See the `.notes` column.

