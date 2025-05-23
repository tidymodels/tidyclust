% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_cluster_assignment.R
\name{extract_cluster_assignment}
\alias{extract_cluster_assignment}
\title{Extract cluster assignments from model}
\usage{
extract_cluster_assignment(object, ...)
}
\arguments{
\item{object}{An fitted \code{\link{cluster_spec}} object.}

\item{...}{Other arguments passed to methods. Using the \code{prefix} allows you
to change the prefix in the levels of the factor levels.}
}
\value{
A \code{tibble::tibble()} with 1 column named \code{.cluster}. This tibble will
correspond the the training data set.
}
\description{
When applied to a fitted cluster specification, returns a tibble with cluster
assignments of the data used to train the model.
}
\details{
Some model types such as K-means as seen in \code{\link[=k_means]{k_means()}} stores the
cluster assignments in the object itself. leading the use of this function to
act as an simple extract. Other model types such as Hierarchical
(Agglomerative) Clustering as seen in \code{\link[=hier_clust]{hier_clust()}}, are fit in such a way
that the number of clusters can be determined at any time after the fit.
Setting the \code{num_clusters} or \code{cut_height} in this function will be used to
determine the clustering when reported.

The ordering of the clusters is such that the first observation in the
training data set will be in cluster 1, the next observation that doesn't
belong to cluster 1 will be in cluster 2, and so on and forth. As the
ordering of clustering doesn't matter, this is done to avoid identical sets
of clustering having different labels if fit multiple times.
\subsection{Related functions}{

\code{extract_cluster_assignment()} is a part of a trio of functions doing
similar things:
\itemize{
\item \code{\link[=extract_cluster_assignment]{extract_cluster_assignment()}} returns the cluster assignments of the
training observations
\item \code{\link[=extract_centroids]{extract_centroids()}} returns the location of the centroids
\item \code{\link[=predict.cluster_fit]{predict()}} returns the cluster a new
observation belongs to
}
}
}
\examples{
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

kmeans_fit |>
  extract_cluster_assignment()

kmeans_fit |>
  extract_cluster_assignment(prefix = "C_")

# Some models such as `hier_clust()` fits in such a way that you can specify
# the number of clusters after the model is fit
hclust_spec <- hier_clust() |>
  set_engine("stats")

hclust_fit <- fit(hclust_spec, ~., mtcars)

hclust_fit |>
  extract_cluster_assignment(num_clusters = 2)

hclust_fit |>
  extract_cluster_assignment(cut_height = 250)
}
\seealso{
\code{\link[=extract_centroids]{extract_centroids()}} \code{\link[=predict.cluster_fit]{predict.cluster_fit()}}
}
