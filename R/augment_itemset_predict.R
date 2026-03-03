#' Augment Itemset Predictions with Truth Values
#'
#' This function processes the output of a `predict()` call for frequent itemset models
#' and joins it with the corresponding ground truth data. It's designed to prepare
#' the prediction and truth values in a format suitable for calculating evaluation metrics
#' using packages like `yardstick`.
#'
#' @param pred_output A data frame that is the output of `predict()` from a `freq_itemsets` model.
#'   It is expected to have a column named `.pred_cluster`, where each cell contains
#'   a data frame with prediction details (including `.pred_item`, `.obs_item`, and `item`).
#' @param truth_output A data frame representing the ground truth. It should have a similar
#'   structure to the input data used for prediction, where columns represent items
#'   and rows represent transactions.
#'
#' @details
#' The function first extracts and combines all individual item prediction data frames
#' nested within the `pred_output`. It then filters for items where a prediction was made
#' (i.e., `!is.na(.pred_item)`) and standardizes item names by removing backticks.
#' The `truth_output` is pivoted to a long format to match the structure of the predictions.
#' Finally, an inner join is performed to ensure that only predicted items are included in
#' the final result, aligning predictions with their corresponding true values.
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item `item`: The name of the item.
#'     \item `row_id`: An identifier for the transaction (row) from which the prediction came.
#'     \item `preds`: The predicted value for the item (either raw probability or binary prediction).
#'     \item `truth`: The true value for the item from `truth_output`.
#'   }
#'   This output is suitable for direct use with `yardstick` metric functions.
#'
#' @examples
#' toy_df <- data.frame(
#' "beer"    = c(FALSE, TRUE, TRUE, TRUE, FALSE),
#' "milk"    = c(TRUE, FALSE, TRUE, TRUE, TRUE),
#' "bread"   = c(TRUE, TRUE, FALSE, TRUE, TRUE),
#' "diapers" = c(TRUE, TRUE, TRUE, TRUE, TRUE),
#' "eggs"    = c(FALSE, TRUE, FALSE, FALSE, FALSE)
#' )
#'
#' new_data <- data.frame(
#' "beer"    = NA,
#' "milk"    = TRUE,
#' "bread"   = TRUE,
#' "diapers" = TRUE,
#' "eggs"    = FALSE
#' )
#'
#' truth_df <- data.frame(
#' "beer"    = FALSE,
#' "milk"    = TRUE,
#' "bread"   = TRUE,
#' "diapers" = TRUE,
#' "eggs"    = FALSE
#' )
#'
#' fi_spec <- freq_itemsets(
#'  min_support = 0.05,
#'  mining_method = "eclat"
#'  ) |>
#'  set_engine("arules") |>
#'  set_mode("partition")
#'
#' fi_fit <- fi_spec |>
#'  fit(~ .,
#'     data = toy_df
#'  )
#'
#' aug_pred <- fi_fit |>
#'  predict(new_data, type = "raw") |>
#'  augment_itemset_predict(truth_output = truth_df)
#'
#' aug_pred
#'
#' # Example use of formatted output
#' aug_pred |>
#'   yardstick::rmse(truth, preds)
#'
#' @export

augment_itemset_predict <- function(pred_output, truth_output) {
  # Extract all predictions (bind all .pred_cluster dataframes)
  preds_df <- dplyr::bind_rows(pred_output$.pred_cluster, .id = "row_id") %>%
    dplyr::filter(!is.na(.pred_item)) %>%  # Keep only rows with predictions
    dplyr::mutate(
      item = gsub("`|TRUE|FALSE", "", item) # Remove backticks, TRUE, and FALSE from item names
    )
    dplyr::select(row_id, item, preds = .pred_item)  # Standardize column names

  # Pivot truth data to long format (to match predictions)
  truth_long <- truth_output %>%
    tibble::rownames_to_column("row_id") %>%
    tidyr::pivot_longer(
      cols = -row_id,
      names_to = "item",
      values_to = "truth_value"
    ) %>%
    dplyr::mutate(truth_value = as.numeric(truth_value))

  # Join predictions with truth (inner join to keep only predicted items)
  result <- preds_df %>%
    dplyr::inner_join(truth_long, by = c("row_id", "item"))

  # Return simplified output (preds vs truth)
  dplyr::select(result, item, row_id, preds, truth = truth_value)
}

#' Generate Dataframe with Random NAs and Corresponding Truth
#'
#' @description
#' This helper function creates a new data frame by randomly introducing `NA` values
#' into an input data frame. It also returns the original data frame as a "truth"
#' reference, which can be useful for simulating scenarios with missing data
#' for prediction tasks.
#'
#' @param df The input data frame to which `NA` values will be introduced.
#'   It is typically a transactional dataset where columns are items and rows are transactions.
#' @param na_prob The probability (between 0 and 1) that any given cell in the
#'   input data frame will be replaced with `NA`.
#'
#' @return A list containing two data frames:
#'   \itemize{
#'     \item `na_data`: The data frame with `NA` values randomly introduced.
#'     \item `truth`: The original input data frame, serving as the ground truth.
#'   }
#' @examples
#' # Create a sample data frame
#' sample_df <- data.frame(
#'   itemA = c(1, 0, 1),
#'   itemB = c(0, 1, 1),
#'   itemC = c(1, 1, 0)
#' )
#'
#' # Generate NA data and truth with 30% NA probability
#' set.seed(123)
#' na_data_list <- random_na_with_truth(sample_df, na_prob = 0.3)
#'
#' # View the NA data
#' print(na_data_list$na_data)
#'
#' # View the truth data
#' print(na_data_list$truth)
#'
#' This function is not exported as it was used to test and provide examples in
#' the vignettes, it may be formally introduced in the future.
random_na_with_truth <- function(df, na_prob = 0.3) {
  # Create a copy of the original dataframe to store truth values
  truth_df <- df

  # Create a mask of NAs (TRUE = becomes NA)
  na_mask <- matrix(
    sample(
      c(TRUE, FALSE),
      size = nrow(df) * ncol(df),
      replace = TRUE,
      prob = c(na_prob, 1 - na_prob)
    ),
    nrow = nrow(df)
  )

  # Apply the mask to create NA values
  na_df <- df
  na_df[na_mask] <- NA

  # Return both the NA-filled dataframe and the truth
  list(
    na_data = na_df,
    truth = truth_df
  )
}
