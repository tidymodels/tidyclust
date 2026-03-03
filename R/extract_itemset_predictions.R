#' Extract Predictions from Observation Data Frames
#'
#' This function processes a data frame containing observation data frames and extracts non-NA values.
#'
#' Returns recommender predictions with predicted values imputed into dataset
#' Notes: currently imputes thresholded probabilities
#'
#' @param pred_output A data frame with one column, where each cell contains a data frame.
#' @return A data frame with items as columns and non-NA values as rows.
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
#' fi_fit |>
#'  predict(new_data) |>
#'  extract_itemset_predictions()
#'
#' @export

extract_itemset_predictions <- function(pred_output) {
  # Extract the list of data frames from the single column
  data_frames <- pred_output$.pred_cluster

  # Define the function to be passed to reduce instead of using lambda
  processing_function <- function(.x_acc, .y_current) {
    # .x_acc is the accumulated result (the first argument to .f)
    # .y_current is the current data frame from data_frames (the second argument to .f)

    # Process each data frame
    processed <- .y_current %>%
      dplyr::mutate(value = ifelse(!is.na(.obs_item), .obs_item, .pred_item)) %>%
      dplyr::select(item, value) %>%
      tidyr::pivot_wider(names_from = item, values_from = value)

    # Combine the processed data frame with the results
    dplyr::bind_rows(.x_acc, processed)
  }

  # Process each observation and combine results using reduce
  result_df <- reduce(
    .x = data_frames,
    .f = processing_function,
    .init = NULL
  )

  return(result_df)
}
