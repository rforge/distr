\name{readPkgVersion}
\alias{readPkgVersion}

\title{readPkgVersion}
\description{
reads the package version out of the \file{DESCRIPTION} file}
\usage{
readPkgVersion(package, lib.loc = NULL)
}
\arguments{
  \item{package}{character; name of the package}
  \item{lib.loc}{location of a local library in which the described package resides}
  }


\value{
package version as character if existing else ""
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
readPkgVersion(package = "distr")
}

\keyword{utilities}

