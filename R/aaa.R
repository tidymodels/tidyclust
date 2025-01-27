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
    "iteration",
    "lab",
    "name",
    "neighbor",
    "new_data",
    "object",
    "orig_label",
    "original",
    "predictor_indicators",
    "remove_intercept",
    "seed",
    "sil_width",
    "splits",
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
