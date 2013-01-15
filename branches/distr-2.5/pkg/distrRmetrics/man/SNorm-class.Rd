\name{SNorm-class}
\docType{class}
\alias{SNorm-class}
\alias{mean,SNorm-method}
\alias{mean<-,SNorm-method}
\alias{nu,SNorm-method}
\alias{nu<-,SNorm-method}
\alias{sd,SNorm-method}
\alias{sd<-,SNorm-method}
\alias{xi,SNorm-method}
\alias{xi<-,SNorm-method}


\title{SNorm distribution}
\description{The skew normal distribution.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("SNorm", mean, sd,xi)}.
  More frequently they are created via the generating function 
  \code{SNorm}.
}
\section{Slots}{
  \describe{
    \item{\code{img}}{Object of class \code{"Reals"}. }
    \item{\code{param}}{Object of class \code{"SNormParameter"}. }
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
    \item{xi}{\code{signature(object = "SNorm")}: wrapped access method for
      slot \code{xi} of slot \code{param}. }
    \item{mean}{\code{signature(object = "SNorm")}: wrapped access method for
      slot \code{mean} of slot \code{param}. }
    \item{nu}{\code{signature(object = "SNorm")}:  wrapped access method for
      slot \code{nu} of slot \code{param}. }
    \item{sd}{\code{signature(x = "SNorm")}: wrapped access method for
      slot \code{sd} of slot \code{param}. }

    \item{xi<-}{\code{signature(object = "SNorm")}: wrapped replace method for
      slot \code{xi} of slot \code{param}. }
    \item{mean<-}{\code{signature(object = "SNorm")}: wrapped replace  method for
      slot \code{mean} of slot \code{param}. }
    \item{nu<-}{\code{signature(object = "SNorm")}: wrapped replace  method for
      slot \code{nu} of slot \code{param}. }
    \item{sd<-}{\code{signature(x = "SNorm")}: wrapped replace  method for
      slot \code{sd} of slot \code{param}. }

  }
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
\note{This class is based on the code provided by the package \pkg{fGarch}
by  Diethelm Wuertz}
\seealso{\code{\link[fGarch:snorm]{dsnorm}}, \code{\link[distr]{AbscontDistribution-class}}}
\examples{
(SN <- SNorm(xi=2)) # SN is a skewed normal distribution with nu = 3.
set.seed(1)
r(SN)(1) # one random number generated from this distribution, e.g. -0.4037723
d(SN)(1) # Density of this distribution is  0.1914826 for x = 1.
p(SN)(1) # Probability that x < 1 is 0.8374454.
q(SN)(.1) # Probability that x < -1.137878 is 0.1.
xi(SN) # shape of this distribution is 2.
xi(SN) <- 2.5 # shape of this distribution is now 2.5.
plot(SN)
}
\concept{SNorm}
\keyword{distribution}
\concept{extreme value distribution}
\concept{absolutely continuous distribution}
\concept{S4 distribution class}
