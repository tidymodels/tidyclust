% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R, R/extract_parameter_set_dials.R
\name{extract-tidyclust}
\alias{extract-tidyclust}
\alias{extract_fit_engine.cluster_fit}
\alias{extract_parameter_set_dials.cluster_spec}
\title{Extract elements of a tidyclust model object}
\usage{
\method{extract_fit_engine}{cluster_fit}(x, ...)

\method{extract_parameter_set_dials}{cluster_spec}(x, ...)
}
\arguments{
\item{x}{A \code{\link{cluster_fit}} object or a \code{\link{cluster_spec}} object.}

\item{...}{Not currently used.}
}
\value{
The extracted value from the tidyclust object, \code{x}, as described in the
description section.
}
\description{
These functions extract various elements from a clustering object. If they do
not exist yet, an error is thrown.
\itemize{
\item \code{extract_fit_engine()} returns the engine specific fit embedded within
a tidyclust model fit. For example, when using \code{\link[=k_means]{k_means()}}
with the \code{"lm"} engine, this returns the underlying \code{kmeans} object.
\item \code{extract_parameter_set_dials()} returns a set of dials parameter objects.
}
}
\details{
Extracting the underlying engine fit can be helpful for describing the
model (via \code{print()}, \code{summary()}, \code{plot()}, etc.) or for variable
importance/explainers.

However, users should not invoke the
\code{\link[=predict.cluster_fit]{predict()}} method on an extracted model.
There may be preprocessing operations that \code{tidyclust} has executed on the
data prior to giving it to the model. Bypassing these can lead to errors or
silently generating incorrect predictions.

\strong{Good}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{   tidyclust_fit |> predict(new_data)
}\if{html}{\out{</div>}}

\strong{Bad}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{   tidyclust_fit |> extract_fit_engine() |> predict(new_data)
}\if{html}{\out{</div>}}
}
\examples{
kmeans_spec <- k_means(num_clusters = 2)
kmeans_fit <- fit(kmeans_spec, ~., data = mtcars)

extract_fit_engine(kmeans_fit)
}
