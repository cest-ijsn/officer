% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ph_location.R
\name{ph_location_label}
\alias{ph_location_label}
\title{location of a named placeholder}
\usage{
ph_location_label(x, layout = NULL, master = "Office Theme", ph_label)
}
\arguments{
\item{x}{an rpptx object}

\item{layout}{slide layout name to use}

\item{master}{master layout name where \code{layout} is located}

\item{ph_label}{placeholder label. It can be read in PowerPoint or
with function \code{layout_properties()} in column \code{ph_label}.}
}
\description{
The function will use the label of a placeholder
to find the corresponding location.
}
\details{
The location of the bounding box associated to a placeholder
within a presentation slide is specified with the left top coordinate,
the width and the height. These are defined in inches:

\describe{
  \item{offx}{left coordinate of the bounding box}
  \item{offy}{top coordinate of the bounding box}
  \item{width}{width of the bounding box}
  \item{height}{height of the bounding box}
}

In addition to these attributes, there is also an attribute \code{ph_label}
that can be associated with the shape (shapes, text boxes, images and other objects
will be identified with that label in the Selection Pane of PowerPoint).

This result can then be used to position new elements in the slides with
functions \code{ph_with_*}.
}
\examples{
x <- read_pptx()
ph_location_label(x, layout = "Title and Content",
  ph_label = "Content Placeholder 2")
}
\seealso{
Other functions for placeholder location: \code{\link{ph_location_fullsize}},
  \code{\link{ph_location_left}},
  \code{\link{ph_location_right}},
  \code{\link{ph_location_type}}, \code{\link{ph_location}}
}
\concept{functions for placeholder location}