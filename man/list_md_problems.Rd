% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engine_docs.R
\name{list_md_problems}
\alias{list_md_problems}
\title{Locate and show errors/warnings in engine-specific documentation}
\usage{
list_md_problems()
}
\value{
A tibble with column \code{file} for the file name, \code{line} indicating
the line where the error/warning occurred, and \code{problem} showing the
error/warning message.
}
\description{
Locate and show errors/warnings in engine-specific documentation
}
\keyword{internal}
