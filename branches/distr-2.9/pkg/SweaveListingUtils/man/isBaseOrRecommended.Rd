\name{isBaseOrRecommended}
\alias{isBaseOrRecommended}
\title{isBaseOrRecommended}
\description{
finds out whether a package is of priority \code{base} and \code{recommended}.}

\usage{
isBaseOrRecommended(pkgs)
}

\arguments{
  \item{pkgs}{character; the packages for which we want to find out if
              it is of priority \code{base} and \code{recommended}.}
}

\value{
 a logical of the same length as the input
}

\details{
reads out the \file{DESCRIPTION} files of the packages to
get the information.
}

\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\examples{
isBaseOrRecommended(pkgs = c("SweaveListingUtils","Matrix","splines"))}
\keyword{utilities}
\keyword{internal}
