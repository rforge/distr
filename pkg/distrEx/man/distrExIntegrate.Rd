\name{distrExIntegrate}
\alias{distrExIntegrate}

\title{Integration of One-Dimensional Functions}
\description{
  Numerical integration via \code{integrate}. In case \code{integrate}
  fails a Gauss-Legendre quadrature is performed.
}
\usage{
distrExIntegrate(f, lower, upper, subdivisions = 100, 
                 rel.tol = .Machine$double.eps^0.25, 
                 abs.tol = rel.tol, stop.on.error = TRUE, 
                 distr, order, ...)
}

\arguments{
  \item{f}{ an R function taking a numeric first argument and returning a
            numeric vector of the same length.  Returning a non-finite
            element will generate an error. }
  \item{lower}{ lower limit of integration. Can be \code{-Inf}. }
  \item{upper}{ upper limit of integration. Can be \code{Inf}. }
  \item{subdivisions}{ the maximum number of subintervals. }
  \item{rel.tol}{ relative accuracy requested. }
  \item{abs.tol}{ absolute accuracy requested. }
  \item{stop.on.error}{ logical. If \code{TRUE} (the default) an error 
    stops the function. If false some errors will give a result with a 
    warning in the \code{message} component. }
  \item{distr}{ object of class \code{UnivariateDistribution}. }
  \item{order}{ order of Gauss-Legendre quadrature. }
  \item{\dots}{ additional arguments to be passed to \code{f}. Remember 
    to use argument names not matching those of \code{integrate}
    and \code{GLIntegrate}! }
}
\details{
  This function calls \code{integrate}. In case \code{integrate}
  returns an error a Gauss-Legendre integration is performed using
  \code{GLIntegrate}. If \code{lower} or (and) \code{upper} are infinite
  the \code{GLIntegrateTruncQuantile}, respectively the 
  \code{1-GLIntegrateTruncQuantile} quantile of \code{distr} is used 
  instead.
}
\value{Estimate of the integral.}
\references{ 
  Based on QUADPACK routines \code{dqags} and \code{dqagi} by
  R. Piessens and E. deDoncker-Kapenga, available from Netlib.

  R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner (1983)
  \emph{Quadpack: a Subroutine Package for Automatic Integration}.
  Springer Verlag.

  W.H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery (1992)
  \emph{Numerical Recipies in C}. The Art of Scientific Computing.
  Second Edition. Cambridge University Press.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{\code{\link[stats]{integrate}}, \code{\link{GLIntegrate}}, 
\code{\link{distrExOptions}}}
\examples{
fkt <- function(x){x*dchisq(x+1, df = 1)}
integrate(fkt, lower = -1, upper = 3)
GLIntegrate(fkt, lower = -1, upper = 3)
try(integrate(fkt, lower = -1, upper = 5))
distrExIntegrate(fkt, lower = -1, upper = 5)
}
\concept{integration}
\keyword{math}
\keyword{utilities}
