\name{setBaseOrRecommended}
\alias{setBaseOrRecommended}
\title{setBaseOrRecommended}
\description{
modifies the color settings in keywordstyles of base and recommended packages.}

\usage{
setBaseOrRecommended(pkgs,kws)
}

\arguments{
  \item{pkgs}{character; the packages for which we want to find out if
              it is of priority \code{base} and \code{recommended}.}
  \item{kws}{character; the keywordstyles for the packages to be modified.}
}

\value{
 the modified keywordstyle vector.
}


\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\keyword{utilities}
\keyword{internal}