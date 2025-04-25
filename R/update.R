#' Update a cluster specification
#'
#' @description If parameters of a cluster specification need to be modified,
#'   `update()` can be used in lieu of recreating the object from scratch.
#'
#' @inheritParams k_means
#' @inheritParams hier_clust
#' @param object A cluster specification.
#' @param parameters A 1-row tibble or named list with _main_ parameters to
#'   update. Use **either** `parameters` **or** the main arguments directly when
#'   updating. If the main arguments are used, these will supersede the values
#'   in `parameters`. Also, using engine arguments in this object will result in
#'   an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be modified in-place
#'   or replaced wholesale.
#' @return An updated cluster specification.
#' @name tidyclust_update
#' @examples
#' kmeans_spec <- k_means(num_clusters = 5)
#' kmeans_spec
#' update(kmeans_spec, num_clusters = 1)
#' update(kmeans_spec, num_clusters = 1, fresh = TRUE)
#'
#' param_values <- tibble::tibble(num_clusters = 10)
#'
#' kmeans_spec |> update(param_values)
NULL
