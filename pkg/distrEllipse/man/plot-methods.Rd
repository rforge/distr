\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,SphericalDistribution,missing-method}
\alias{plot,MultivarMixingDistribution,missing-method}
\title{ Methods for Function plot in Package `distrEllipse' }
\description{plot-methods}
\usage{plot(x, y, ...)
\S4method{plot}{SphericalDistribution,missing}(x, Nsim = getdistrEllipseOption("Nsim"), ...,
               withED = getdistrEllipseOption("withED"),
               lwd.Ed = getdistrEllipseOption("lwd.Ed"),
               col.Ed = getdistrEllipseOption("col.Ed"),
               withMean = getdistrEllipseOption("withMean"),
               cex.mean = getdistrEllipseOption("cex.mean"),
               pch.mean = getdistrEllipseOption("pch.mean"),
               col.mean = getdistrEllipseOption("col.mean"))
\S4method{plot}{MultivarMixingDistribution,missing}(x, Nsim = getdistrEllipseOption("Nsim"), ...,
               withED = getdistrEllipseOption("withED"),
               lwd.Ed = getdistrEllipseOption("lwd.Ed"),
               col.Ed = getdistrEllipseOption("col.Ed"),
               withMean = getdistrEllipseOption("withMean"),
               cex.mean = getdistrEllipseOption("cex.mean"),
               pch.mean = getdistrEllipseOption("pch.mean"),
               col.mean = getdistrEllipseOption("col.mean"))
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
               \code{\link[stats]{plot.stepfun}} }
}
\details{
Using \code{pairs}, plots all pairs of coordinates of the object, using simulated
values.
Any parameters of \code{\link[graphics]{pairs}} may be passed on to this particular
\code{plot} method. 
}

\examples{
E0 <- matrix(c(2,1,1,4),2,2)\%*\%EllipticalDistribution()+c(2,1)
E1 <- matrix(c(3,2,2,4),2,2)\%*\%EllipticalDistribution(radDistr = exp(Binom(10,.8)))
plot(E0)
plot(E1, withED=FALSE, Nsim=5000)
mylist <- MultivarMixingDistribution(E0,E1, mixCoeff=c(1/4,3/4))
plot(mylist)
}
\seealso{\code{\link[graphics]{pairs}},
         \code{\link[graphics]{plot}}
         \code{\link[graphics]{plot.default}}, 
         \code{\link[stats]{plot.stepfun}},  \code{\link[graphics]{par}}}
\keyword{methods}
\keyword{hplot}
\keyword{distribution}

