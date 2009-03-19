\name{readSourceFromRForge}
\alias{readSourceFromRForge}

\title{readSourceFromRForge}
\description{
reads a source file (usually \file{.R} oder \file{.Rd})
from R forge repository}
\usage{
readSourceFromRForge(PKG, TYPE, FILENAME, PROJECT,
                                 fromRForge = getSweaveListingOption("fromRForge"),
                                 base.url = getSweaveListingOption("base.url"))
}
\arguments{
  \item{PKG}{character; name of package to be downloaded}
  \item{TYPE}{character; style of the source code --- "man" or "R"}
  \item{FILENAME}{character; the name of the source file to be downloaded}
  \item{PROJECT}{character; the name of the R-Forge project}
  \item{fromRForge}{logical; shall code be downloaded from an R-Forge mirror?
                    Defaults to the corresponding global option}
  \item{base.url}{character; base url from where to download the code sniplet}
  }

\details{
reads source file from R-forge [from most recent revision in trunc],
but caches the result in a global list in the namespace of package 
\pkg{SweaveListingUtils}, so if the URL has already been cached, the 
cached copy is used.
The cached copies are stored in list \code{.CacheFiles};
the current length of this list is stored in \code{.CacheLength}.
}

\value{
the character content of the source file
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
readSourceFromRForge("distr","R","AllClasses.R","distr")
}

\keyword{utilities}


