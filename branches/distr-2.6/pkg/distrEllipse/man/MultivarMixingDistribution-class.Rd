\name{MultivarMixingDistribution-class}
\docType{class}
\alias{MultivarMixingDistribution-class}
\alias{mixCoeff}
\alias{mixDistr}
\alias{mixCoeff<-}
\alias{mixDistr<-}
\alias{mixCoeff<--methods}
\alias{mixDistr<--methods}
\alias{mixCoeff<-,MultivarMixingDistribution-method}
\alias{mixDistr<-,MultivarMixingDistribution-method}
\alias{mixCoeff-methods}
\alias{mixDistr-methods}
\alias{mixCoeff,MultivarMixingDistribution-method}
\alias{mixDistr,MultivarMixingDistribution-method}
\alias{support,MultivarMixingDistribution-method}
\alias{gaps,MultivarMixingDistribution-method}
\alias{.logExact,MultivarMixingDistribution-method}
\alias{.lowerExact,MultivarMixingDistribution-method}
\alias{Symmetry,MultivarMixingDistribution-method}
\alias{dimension,MultivarMixingDistribution-method}
\alias{dim,MultivarMixingDistribution-method}
\alias{E,MultivarMixingDistribution,ANY,ANY-method}
\alias{show,MultivarMixingDistribution-method}
\alias{showobj,MultivarMixingDistribution-method}

\title{Class "MultivarMixingDistribution"}
\description{\code{MultivarMixingDistribution}-class is a class to formalize 
                   multivariate mixing distributions; it is a subclass to
                   class \code{MultivariateDistribution}.}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("MultivarMixingDistribution", ...)}.
  More frequently they are created via the generating function 
  \code{\link{MultivarMixingDistribution}}.
}
\section{Slots}{
  \describe{
    \item{\code{mixCoeff}}{Object of class \code{"numeric"}: a vector of 
            probabilities for the mixing components.}
    \item{\code{mixDistr}}{Object of class \code{"MultivarDistrList"}: a list of
    multivariate distributions containing the mixing components; must be of same
    length as \code{mixCoeff}.}
    \item{\code{img}}{Object of class \code{"Reals"}: the space of the image of this distribution which has dimension 1
    and the name "Real Space" }
    \item{\code{param}}{Object of class \code{"Parameter"}: the parameter of this distribution, having only the
    slot name "Parameter of a discrete distribution" }
    \item{\code{r}}{Object of class \code{"function"}: generates random numbers}
    \item{\code{d}}{fixed to \code{NULL}}
    \item{\code{p}}{Object of class \code{"OptionalFunction"}: if non-null cumulative distribution function}
    \item{\code{q}}{Object of class \code{"OptionalFunction"}: if non-null quantile function}
    \item{\code{.withArith}}{logical: used internally to issue warnings as to interpretation of arithmetics}
    \item{\code{.withSim}}{logical: used internally to issue warnings as to accuracy}
    \item{\code{.logExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              log version of density, cdf, and quantile function}
    \item{\code{.lowerExact}}{logical: used internally to flag the case where there are explicit formulae for the
                              lower tail version of cdf and quantile function}
    \item{\code{Symmetry}}{object of class \code{"DistributionSymmetry"};
     used internally to avoid unnecessary calculations.}
  }
}
\section{Extends}{
Class \code{"MultivariateDistribution"}
class \code{"Distribution"} by class \code{"MultivariateDistribution"}.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "MultivarMixingDistribution")} prints the object}
    \item{mixCoeff<-}{\code{signature(object = "MultivarMixingDistribution")} replaces the corresponding slot}
    \item{mixCoeff}{\code{signature(object = "MultivarMixingDistribution")} returns the corresponding slot}
    \item{mixDistr<-}{\code{signature(object = "MultivarMixingDistribution")} replaces the corresponding slot}
    \item{mixDistr}{\code{signature(object = "MultivarMixingDistribution")} returns the corresponding slot}
    \item{support}{\code{signature(object = "MultivarMixingDistribution")} returns the corresponding slot}
    \item{gaps}{\code{signature(object = "MultivarMixingDistribution")} returns the corresponding slot}
    \item{.logExact}{\code{signature(object = "Distribution")}: returns slot 
      \code{.logExact} if existing; else tries to convert the object to a newer 
      version of its class by \code{\link[distr:versionmanagement]{conv2NewVersion}} and
      returns the corresponding slot of the converted object.}
    \item{.lowerExact}{\code{signature(object = "Distribution")}: returns slot 
       \code{.lowerExact} if existing; else tries to convert the object to a 
       newer version of its class by \code{\link[distr:versionmanagement]{conv2NewVersion}} and
     returns the corresponding slot of the converted object.}
    \item{Symmetry}{returns slot \code{Symmetry} if existing; else
    tries to convert the object to a 
       newer version of its class by \code{\link[distr:versionmanagement]{conv2NewVersion}} and
     returns the corresponding slot of the converted object.}
    \item{plot}{\code{signature(x = "MultivarMixingDistribution", y = "missing")}:
      plot for an spherically symmetric distribution; see \code{\link{plot-methods}}.
    }
    \item{E}{corresponding expectation --- see \code{\link[distrEx]{E}}.}
    \item{dimension}{dim of the range space.}
    \item{dim}{synonym to dimension.}
    \item{show}{\code{signature(object = "MultivarMixingDistribution")}:
      \code{show} method for spherically symmetric distributions.
    }
    \item{showobj}{\code{signature(object = "MultivarMixingDistribution")}:
      \code{showobj} method for spherically symmetric distributions.
    }
  }
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
  }

\seealso{
\code{\link[distr]{Parameter-class}},
\code{\link[distrEx]{MultivariateDistribution-class}},
\code{\link[distr]{LatticeDistribution-class}},
\code{\link[distr]{AbscontDistribution-class}},
\code{\link[distr]{simplifyD-methods}},
\code{\link[distr]{flat.mix}}
}
\examples{
mylist <- MultivarMixingDistribution(Binom(3,.3), Dirac(2), Norm(), 
          mixCoeff=c(1/4,1/5,11/20))
mylist2 <- MultivarMixingDistribution(Binom(3,.3), mylist, 
          mixCoeff=c(.3,.7))
mylist2
p(mylist)(0.3)          
mixDistr(mylist2)

##multivariate
E1 <- diag(1,2)\%*\%EllipticalDistribution(radDistr=Gammad())+c(1,2)
mylistD <- MultivarMixingDistribution(MVNorm(), E1, MVt(),
          mixCoeff=c(1/4,1/5,11/20))
mylistD2 <- MultivarMixingDistribution(E1+c(-2,2), mylistD,
          mixCoeff=c(.3,.7))
mylistD2
p(mylistD)
mixDistr(mylistD2)
}
\keyword{distribution}
\concept{discrete distribution}
\concept{lattice distribution}
\concept{support of a distribution}
\concept{S4 distribution class}
\concept{generating function}
