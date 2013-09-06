\name{SSTd-class}
\docType{class}
\alias{SSTd-class}
\alias{mean,SSTd-method}
\alias{mean<-,SSTd-method}
\alias{nu}
\alias{nu-methods}
\alias{nu<-}
\alias{nu<--methods}
\alias{nu,SSTd-method}
\alias{nu<-,SSTd-method}
\alias{sd,SSTd-method}
\alias{sd<-,SSTd-method}
\alias{xi}
\alias{xi-methods}
\alias{xi<-}
\alias{xi<--methods}
\alias{xi,SSTd-method}
\alias{xi<-,SSTd-method}


\title{SSTd distribution}
\description{The standardized skew Student-t distribution.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("SSTd", mean, sd,xi)}.
  More frequently they are created via the generating function 
  \code{SSTd}.
}
\section{Slots}{
  \describe{
    \item{\code{img}}{Object of class \code{"Reals"}. }
    \item{\code{param}}{Object of class \code{"SSTdParameter"}. }
    \item{\code{r}}{\code{rgpd}}
    \item{\code{d}}{\code{dgpd}}
    \item{\code{p}}{\code{pgpd}, but vectorized and with special treatment of 
             arguments \code{lower.tail} and \code{log.p}}
    \item{\code{q}}{\code{qgpd}, but vectorized and with special treatment of 
             arguments \code{lower.tail} and \code{log.p}}
    \item{\code{gaps}}{(numeric) matrix or \code{NULL}}
    \item{\code{.withArith}}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}}{logical: used internally to issue warnings as to accuracy}
    \item{\code{.logExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              log version of density, cdf, and quantile function}
    \item{\code{.lowerExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              lower tail version of cdf and quantile function}
  }
}
\section{Extends}{
Class \code{"AbscontDistribution"}, directly.\cr
Class \code{"UnivariateDistribution"}, by class \code{"AbscontDistribution"}.\cr
Class \code{"Distribution"}, by class \code{"AbscontDistribution"}.
}
\section{Methods}{
  \describe{
    \item{xi}{\code{signature(object = "SSTd")}: wrapped access method for
      slot \code{xi} of slot \code{param}. }
    \item{mean}{\code{signature(object = "SSTd")}: wrapped access method for
      slot \code{mean} of slot \code{param}. }
    \item{nu}{\code{signature(object = "SSTd")}:  wrapped access method for
      slot \code{nu} of slot \code{param}. }
    \item{sd}{\code{signature(x = "SSTd")}: wrapped access method for
      slot \code{sd} of slot \code{param}. }

    \item{xi<-}{\code{signature(object = "SSTd")}: wrapped replace method for
      slot \code{xi} of slot \code{param}. }
    \item{mean<-}{\code{signature(object = "SSTd")}: wrapped replace  method for
      slot \code{mean} of slot \code{param}. }
    \item{nu<-}{\code{signature(object = "SSTd")}: wrapped replace  method for
      slot \code{nu} of slot \code{param}. }
    \item{sd<-}{\code{signature(x = "SSTd")}: wrapped replace  method for
      slot \code{sd} of slot \code{param}. }

  }
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
\note{This class is based on the code provided by the package \pkg{fGarch}
by  Diethelm Wuertz}
\seealso{\code{\link[fGarch:dist-sstd]{dsstd}}, \code{\link[distr]{AbscontDistribution-class}}}
\examples{
(ST <- SSTd(xi=2, nu = 3)) # ST is a skewed t distribution with xi = 2 and nu = 3.
set.seed(1)
r(ST)(1) # one random number generated from this distribution, e.g. -0.4432824
d(ST)(1) # Density of this distribution is 0.1204624 for x = 1.
p(ST)(1) # Probability that x < 1 is 0.9035449.
q(ST)(.1) # Probability that x < -0.4432824 is 0.1.
nu(ST) # df of this distribution is 3.
nu(ST) <- 4 # df of this distribution is now 4.
plot(ST)
}
\concept{SSTd}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
