\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,SphericalDistribution,missing-method}
\title{ Methods for Function plot in Package `distrEllipse' }
\description{plot-methods}
\usage{plot(x, y, ...)
\S4method{plot}{SphericalDistribution,missing}(x, Nsim = 2000, ...,
                withED = TRUE, lwd.Ed = 2, col.Ed = c(3,4),
                withMean = TRUE, cex.mean = 2, pch.mean = 20, col.mean = 2)
}
\arguments{
  \item{x}{object of class 
            \code{"SphericalDistribution"} distribution to be plotted }
  \item{y}{ missing }
  \item{Nsim}{number of (simulated) points to be plotted.}
  
  \item{withED}{logical; shall principal axes of the contour ellipsoid be plot in
                 (for each panel)?}
  \item{lwd.Ed}{line width of principal axes  (for each panel).}
  \item{col.Ed}{color of principal axes  (for each panel).}
  \item{withMean}{logical; shall mean  be plot in  (for each panel)?}
  \item{cex.mean}{size of the mean symbol  (for each panel).}
  \item{pch.mean}{mean symbol  (for each panel).}
  \item{col.mean}{color of the mean symbol (for each panel).}
  \item{\dots}{addtional arguments for \code{plot} --- see 
               \code{\link[graphics]{plot}}, 
               \code{\link[graphics]{plot.default}}, 
               \code{\link[graphics]{plot.stepfun}} }
}
\details{
Using \code{pairs}, plots all pairs of coordinates of the object, using simulated
values.
Any parameters of \code{\link[graphics]{pairs}} may be passed on to this particular
\code{plot} method. 
}

\examples{
}
\seealso{\code{\link[graphics]{pairs}},
         \code{\link[graphics]{plot}}
         \code{\link[graphics]{plot.default}}, 
         \code{\link[graphics]{plot.stepfun}},  \code{\link[graphics]{par}}}
\keyword{methods}
\keyword{hplot}
\keyword{distribution}

