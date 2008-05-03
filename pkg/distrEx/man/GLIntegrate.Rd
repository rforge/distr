\name{GLIntegrate}
\alias{GLIntegrate}

\title{Gauss-Legendre Quadrature}
\description{
  Gauss-Legendre quadrature over a finite interval.
}
\usage{
GLIntegrate(f, lower, upper, order = 500, ...)
}
\arguments{
  \item{f}{ an R function taking a numeric first argument and returning a
            numeric vector of the same length.  Returning a non-finite
            element will generate an error. }
  \item{lower}{ finite lower limit of integration. }
  \item{upper}{ finite upper limit of integration. }
  \item{order}{ order of Gauss-Legendre quadrature. }
  \item{\dots}{ additional arguments to be passed to \code{f}. Remember 
    to use argument names not matching those of \code{GLIntegrate}! }
}
\details{
  In case \code{order = 100, 500, 1000} saved abscissas and weights 
  are used. Otherwise the corresponding abscissas and weights are
  computed using the algorithm given in Section 4.5 of
  Press et al. (1992). 
}
\value{Estimate of the integral.}
\references{ 
  W.H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery (1992)
  \emph{Numerical Recipies in C}. The Art of Scientific Computing.
  Second Edition. Cambridge University Press.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{\code{\link[stats]{integrate}}, \code{\link{distrExIntegrate}}}
\examples{
integrate(dnorm, -1.96, 1.96)
GLIntegrate(dnorm, -1.96, 1.96)
}
\concept{integration}
\keyword{math}
\keyword{utilities}
