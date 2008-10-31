\name{SweaveListingUtils-package}
\alias{SweaveListingUtils-package}
\alias{SweaveListingUtils}
\docType{package}
\title{
Package SweaveListingUtils
}
\description{
\pkg{SweaveListingUtils} contains 
provides utilities for defining R / Rd as TeX-package-listings 
"language" and including R / Rd source file (sniplets) copied
from R-forge in its most recent version thereby avoiding
inconsistencies between vignette and documented source code  
}
\details{
\tabular{ll}{
Package: \tab SweaveListingUtils\cr
Version: \tab 0.1\cr
Date: \tab 2008-10-31\cr
Depends: \tab R(>= 2.7.0)\cr
LazyLoad: \tab yes\cr
License: \tab LGPL-3\cr
}}


\section{Functions}{
\preformatted{
lstset
lstsetR
lstsetRd
SweaveListingPreparations
readSourceFromRForge
copySourceFromRForge
lstinputSourceFromRForge
readPkgVersion
SweaveListingOptions
getSweaveListingOption
SweaveListingoptions
}
}
\section{S3 classes and methods}{
\preformatted{
taglist
print.taglist
}
}

\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}\cr
\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
}

\keyword{package}
\keyword{utilities}
