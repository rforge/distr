\name{liesInSupport}
\alias{liesInSupport,DiscreteMVDistribution,numeric-method}
\alias{liesInSupport,DiscreteMVDistribution,matrix-method}

\title{Generic Function for Testing the Support of a Distribution }
\description{
  The function tests if \code{x} lies in the support of the
  distribution \code{object}.
}
\usage{
\S4method{liesInSupport}{DiscreteMVDistribution,numeric}(object, x, checkFin = FALSE)
\S4method{liesInSupport}{DiscreteMVDistribution,matrix}(object, x, checkFin = FALSE)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"} }
  \item{x}{ numeric vector or matrix }
  \item{checkFin}{ logical: in case \code{FALSE}, we simply check whether
  \code{x} lies exactly in the \emph{numerical} support (of finitely many
  support points); later on we might try to mimick the univariate case
  more closely in case \code{TRUE}, but so far this is not yet used.}
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
