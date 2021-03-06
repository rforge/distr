\name{UnivarMixingDistribution-class}
\docType{class}
\alias{UnivarMixingDistribution-class}
\alias{mixCoeff}
\alias{mixDistr}
\alias{mixCoeff<-}
\alias{mixDistr<-}
\alias{mixCoeff<--methods}
\alias{mixDistr<--methods}
\alias{mixCoeff<-,UnivarMixingDistribution-method}
\alias{mixDistr<-,UnivarMixingDistribution-method}
\alias{mixCoeff-methods}
\alias{mixDistr-methods}
\alias{mixCoeff,UnivarMixingDistribution-method}
\alias{mixDistr,UnivarMixingDistribution-method}

\title{Class "UnivarMixingDistribution"}
\description{\code{UnivarMixingDistribution}-class is a class to formalize 
                   univariate mixing distributions; it is a subclass to
                   class \code{UnivariateDistribution}.}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("UnivarMixingDistribution", ...)}.
  More frequently they are created via the generating function 
  \code{\link{UnivarMixingDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{mixCoeff}:}{Object of class \code{"numeric"}: a vector of 
            probabilities for the mixing components.}
    \item{\code{mixDistr}:}{Object of class \code{"UnivarDistrList"}: a list of
    univariate distributions containing the mixing components; must be of same
    length as \code{mixCoeff}.}
    \item{\code{img}:}{Object of class \code{"Reals"}: the space of the image of this distribution which has dimension 1
    and the name "Real Space" }
    \item{\code{param}:}{Object of class \code{"Parameter"}: the parameter of this distribution, having only the
    slot name "Parameter of a discrete distribution" }
    \item{\code{r}:}{Object of class \code{"function"}: generates random numbers}
    \item{\code{d}:}{fixed to \code{NULL}}
    \item{\code{p}:}{Object of class \code{"function"}: cumulative distribution function}
    \item{\code{q}:}{Object of class \code{"function"}: quantile function}
    \item{\code{.withArith}:}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}:}{logical: used internally to issue warnings as to accuracy}
  }
}
\section{Extends}{
Class \code{"UnivariateDistribution"}
class \code{"Distribution"} by class \code{"UnivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "UnivarMixingDistribution")} prints the object}
    \item{mixCoeff<-}{\code{signature(object = "UnivarMixingDistribution")} replaces the corresponding slot}
    \item{mixCoeff}{\code{signature(object = "UnivarMixingDistribution")} returns the corresponding slot}
    \item{mixDistr<-}{\code{signature(object = "UnivarMixingDistribution")} replaces the corresponding slot}
    \item{mixDistr}{\code{signature(object = "UnivarMixingDistribution")} returns the corresponding slot}
  }
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
  }

\seealso{
\code{\link{Parameter-class}}, 
\code{\link{UnivariateDistribution-class}}, 
\code{\link{LatticeDistribution-class}}, 
\code{\link{AbscontDistribution-class}}, 
\code{\link{simplifyD}}, 
\code{\link{flat.mix}}
}
\examples{
mylist <- UnivarMixingDistribution(Binom(3,.3), Dirac(2), Norm(), 
          mixCoeff=c(1/4,1/5,11/20))
mylist2 <- UnivarMixingDistribution(Binom(3,.3), mylist, 
          mixCoeff=c(.3,.7))
mylist2
p(mylist)(0.3)          
mixDistr(mylist2)
}
\keyword{distribution}
\concept{discrete distribution}
\concept{lattice distribution}
\concept{support of a distribution}
\concept{S4 distribution class}
\concept{generating function}
