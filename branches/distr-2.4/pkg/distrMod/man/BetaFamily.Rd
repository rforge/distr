\name{BetaFamily}
\alias{BetaFamily}

\title{Generating function for Beta families}
\description{
  Generates an object of class \code{"L2ParamFamily"} which
  represents a Beta family.
}
\usage{
BetaFamily(shape1 = 1, shape2 = 1, trafo)
}
\arguments{
  \item{shape1}{ positive real: shape1 parameter }
  \item{shape2}{ positive real: shape2 parameter }
  \item{trafo}{ matrix: transformation of the parameter }
}
\details{
  The slots of the corresponding L2 differentiable 
  parameteric family are filled.
}
\value{Object of class \code{"L2ParamFamily"}}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{L2ParamFamily-class}}, \code{\link[distr]{Beta-class}}}
\examples{
(B1 <- BetaFamily())
FisherInfo(B1)
checkL2deriv(B1)
}
\concept{Beta model}
\keyword{models}
