% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/page_size.R
\name{page_types}
\alias{page_types}
\alias{page_dim}
\title{Supported Named Page \code{TypesList} supported named page types}
\usage{
page_types()

page_dim(page_type)
}
\arguments{
\item{page_type}{character(1). The name of a page size specification. Call
\code{page_types} for supported values.}
}
\value{
for \code{page_types} a character vector of supported page types,
for \code{page_dim} the dimensions (width, then height) of the selected page type.
}
\description{
Supported Named Page \code{TypesList} supported named page types
}
\examples{
page_types()
page_dim("a4")
}
