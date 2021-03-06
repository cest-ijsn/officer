% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppt_ph_with_methods.R
\name{location_eval}
\alias{location_eval}
\title{Utility to eval a location}
\usage{
location_eval(location, x)
}
\arguments{
\item{location}{a location for a placeholder.}

\item{x}{an rpptx object}
}
\description{
Eval a shape location with fortify_location.
This function will be removed in the next release; it was
required when location was a quosure but this is no more
necessary.
}
\seealso{
\code{\link{ph_location}}, \code{\link{ph_with}}
}
