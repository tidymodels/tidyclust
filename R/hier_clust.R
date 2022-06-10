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
#' @param method the agglomeration method to be used. This should be (an
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
           method = "complete") {
    args <- list(
      k = enquo(k),
      h = enquo(h),
      method = enquo(method)
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

#' @export
translate_celery.hier_clust <- function(x, engine = x$engine, ...) {
  x <- translate_celery.default(x, engine, ...)
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
#' @param method the agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
#' `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA), `"median"`
#' (= WPGMC) or `"centroid"` (= UPGMC).
#' @param dist_fun A distance function to use
#'
#' @return A dendrogram
#' @keywords internal
#' @export
hclust_fit <- function(x, k = NULL, h = NULL,
                       method = "complete",
                       dist_fun = Rfast::Dist) {
  dmat <- dist_fun(x)
  res <- hclust(as.dist(dmat), method = method)
  attr(res, "k") <- k
  attr(res, "h") <- h
  return(res)
}
