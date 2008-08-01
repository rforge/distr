\name{internals_for_distrMod}
\alias{internals_for_distrMod}
\alias{.inArgs}
\alias{.isUnitMatrix}
\alias{.validTrafo}
\alias{.CvMMDCovariance}

\title{Internal functions of package distrMod}

\description{
These functions are used internally by package ``distrMod''.}

\usage{
.inArgs(arg, fct)
.isUnitMatrix(m)
.validTrafo(trafo, dimension)
.CvMMDCovariance(L2Fam, param, mu = distribution(L2Fam), expon = 3, 
                 withplot = FALSE, N = 200,
                 ...)
}


\arguments{
  \item{arg}{a formal argument as character}
  \item{fct}{a function}
  \item{m}{a matrix} 
  \item{trafo}{an object of class \code{MatrixorFunction}}
  \item{dimension}{a numeric}
  \item{L2Fam}{an object of class \code{L2ParamFamily} --- for
  which we want to determine the IC resp. the as. [co]variance of the corresponding
  Minimum CvM estimator}
  \item{param}{an object of class \code{ParamFamParameter}, the parameter value
  at which we want to determine the IC resp. the as. [co]variance of the corresponding
  Minimum CvM estimator}
  \item{mu}{an object of class \code{UnivariateDistribution}: integration 
            measure (resp. distribution) for CvM distance}
  \item{expon}{a numeric: number to exponentiate 
               \code{getdistrOption("TruncQuantile")} 
               to get upper and lower 
               \eqn{\mu}{mu}- resp. \eqn{P_\theta}{P_theta}-quantiles
               for the gridpoints (see below)}
  \item{withplot}{logical: shall we plot corresponding ICs?}
  \item{N}{a numeric: the number of gridpoints for constructing the 
           \eqn{\mu}{mu}- resp. \eqn{P_\theta}{P_theta}-``primitive''
           function}
  \item{\dots}{further argument to be passed through --- so 
              \code{.CvMMDCovariance} can digest more arguments}
}

\details{
\code{.inArgs} checks whether an argument \code{arg} is a formal argument of 
     \code{fct} --- not vectorized.

\code{.isUnitMatrix} checks whether the argument is a unit matrix. 

\code{.validTrafo} checks whether the argument is a valid transformation.

\code{.CvMMDCovariance} determines the IC resp. the as. [co]variance of 
   the corresponding Minimum CvM estimator. Still some checking / optimization /
   improvement needed.
}


\value{
\item{.inArgs}{\code{logical} (length 1)}
\item{.isUnitMatrix}{\code{logical} (length 1)}
\item{.validTrafo}{\code{logical} (length 1)}
\item{.CvMMDCovariance}{corresponding as. [co]variance of 
   the corresponding Minimum CvM estimator}
}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
}

\seealso{
\code{\link{MLEstimator}},
\code{\link{Estimate-class}},
\code{\link{MCEstimate-class}},
\code{\link{Confint-class}},
\code{\link{ParamFamParameter-class}}
}
\keyword{internal}
\concept{utilities}
