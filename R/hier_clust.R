#' Hierarchical (Agglomerative) Clustering
#'
#' @description
#'
#' `hier_clust()` defines a model that fits clusters based on a distance-based
#' dendrogram
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"stats"`.
#' @param k Positive integer, number of clusters in model (optional).
#' @param h Positive double, height at which to cut dendrogram to obtain cluster
#' assignments (only used if `k` is `NULL`)
#' @param linkage_method the agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#' `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"`
#' (= WPGMC) or `"centroid"` (= UPGMC).
#' @param dist_fun A distance function to use
#'
#' @examples
#' # show_engines("hier_clust")
#'
#' hier_clust()
#' @export
hier_clust <-
  function(mode = "partition",
           engine = "stats",
           k = NULL,
           h = NULL,
           linkage_method = "complete") {
    args <- list(
      k = enquo(k),
      h = enquo(h),
      linkage_method = enquo(linkage_method)
    )

    new_cluster_spec(
      "hier_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.hier_clust <- function(x, ...) {
  cat("Hierarchical Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update hier_clust
#' @rdname tidyclust_update
#' @export
update.hier_clust <- function(object,
                              parameters = NULL,
                              k = NULL,
                              h = NULL,
                              linkage_method = NULL,
                              fresh = FALSE, ...) {

  eng_args <- parsnip::update_engine_parameters(object$eng_args, ...)

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    k = enquo(k),
    h = enquo(h),
    linkage_method = enquo(linkage_method)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
    if (length(eng_args) > 0)
      object$eng_args[names(eng_args)] <- eng_args
  }

  new_cluster_spec(
    "hier_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' @export
translate_tidyclust.hier_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around hclust function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `stats::hclust` and retains the parameters `k` or `h` as an attribute.
#'
#' @param x matrix or data frame
#' @param k the number of clusters
#' @param h the height to cut the dendrogram
#' @param linkage_method the agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#' `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"`
#' (= WPGMC) or `"centroid"` (= UPGMC).
#' @param dist_fun A distance function to use
#'
#' @return A dendrogram
#' @keywords internal
#' @export
hclust_fit <- function(x, k = NULL, cut_height = NULL,
                       linkage_method = NULL,
                       dist_fun = Rfast::Dist) {
  dmat <- dist_fun(x)
  res <- hclust(as.dist(dmat), method = linkage_method)
  attr(res, "k") <- k
  attr(res, "cut_height") <- cut_height
  attr(res, "training_data") <- x
  return(res)
}
