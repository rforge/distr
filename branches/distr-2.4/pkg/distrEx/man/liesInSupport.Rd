\name{liesInSupport}
\alias{liesInSupport,DiscreteMVDistribution,numeric-method}
\alias{liesInSupport,DiscreteMVDistribution,matrix-method}

\title{Generic Function for Testing the Support of a Distribution }
\description{
  The function tests if \code{x} lies in the support of the
  distribution \code{object}.
}
\usage{
\S4method{liesInSupport}{DiscreteMVDistribution,numeric}(object, x)
\S4method{liesInSupport}{DiscreteMVDistribution,matrix}(object, x)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"} }
  \item{x}{ numeric vector or matrix }
}
%\details{}
\value{logical vector}
\section{Methods}{
\describe{
  \item{object = "DiscreteMVDistribution", x = "numeric":}{ 
    does \code{x} lie in the support of \code{object}. }

  \item{object = "DiscreteMVDistribution", x = "matrix":}{ 
    does \code{x} lie in the support of \code{object}. }
}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{Distribution-class}}}
\examples{
M <- matrix(rpois(30, lambda = 10), ncol = 3)
D1 <- DiscreteMVDistribution(M)
M1 <- rbind(r(D1)(10), matrix(rpois(30, lam = 10), ncol = 3))
liesInSupport(D1, M1)
}
\keyword{distribution}
\keyword{utilities}
\keyword{methods}
\concept{support}
