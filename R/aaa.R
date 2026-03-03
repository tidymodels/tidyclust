# ------------------------------------------------------------------------------
# nocov start

utils::globalVariables(
  c(
    ".",
    "..object",
    ".cluster",
    ".iter_config",
    ".iter_model",
    ".iter_preprocessor",
    ".msg_model",
    ".pred_item",
    ".submodels",
    "call_info",
    "cluster",
    "component",
    "component_id",
    "compute_intercept",
    "data",
    "dist",
    "engine",
    "engine2",
    "exposed",
    "func",
    "id",
    "item",
    "iteration",
    "lab",
    "name",
    "neighbor",
    "new_data",
    "object",
    "orig_label",
    "original",
    "predictor_indicators",
    "preds",
    "remove_intercept",
    "row_id",
    "seed",
    "setNames",
    "sil_width",
    "splits",
    "truth_value",
    "tunable",
    "type",
    "value",
    "x",
    "y"
  )
)

release_bullets <- function() {
  c(
    "Run `knit_engine_docs()` and `devtools::document()` to update docs"
  )
}

# nocov end

# ------------------------------------------------------------------------------

# Initialize model environments

all_modes <- c("partition")

pred_types <- c("cluster", "raw")
