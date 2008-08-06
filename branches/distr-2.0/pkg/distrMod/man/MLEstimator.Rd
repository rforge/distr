\name{MLEstimator}
\alias{MLEstimator}

\title{ Function to compute maximum likelihood estimates }
\description{
  The function \code{MLEstimator} provides a general way to compute 
  maximum likelihood estimates for a given parametric family of 
  probability measures. This is done by calling the function 
  \code{MCEstimator} which minimizes the negative log-Likelihood.
}
\usage{
MLEstimator(x, ParamFamily, startPar = NULL, 
            Infos, trafo = NULL, penalty = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ (empirical) data }
  \item{ParamFamily}{ object of class \code{"ParamFamily"} }
  \item{startPar}{ initial information used by \code{optimize} resp. \code{optim};
    i.e; if (total) parameter is of length 1, \code{startPar} is 
    a search interval, else it is an initial parameter value; if \code{NULL}
    slot \code{startPar} of \code{ParamFamily} is used to produce it;
    in the multivariate case, \code{startPar} may also be of class \code{Estimate},
    in which case slot \code{untransformed.estimate} is used.}
  \item{Infos}{ character: optional informations about estimator }
  \item{trafo}{ an object of class \code{MatrixorFunction} -- a transformation
  for the main parameter}
  \item{penalty}{(non-negative) numeric: penalizes non valid parameter-values}
  \item{\dots}{ further arguments to \code{criterion} or \code{optimize} 
    or \code{optim}, respectively. }
}
\details{
  The function calls \code{MCEstimator} using the negative log-likelihood
  as criterion which should be minimized.
}
\value{
  An object of S4-class \code{"MCEstimate"} which inherits from class 
  \code{"Estimate"}.
}
%\references{  }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{ParamFamily-class}}, \code{\link{ParamFamily}}, 
         \code{\link{MCEstimator}, \code{\link{MCEstimate-class}}},
\code{\link[MASS]{fitdistr}} }
\examples{
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
G <- GammaFamily(scale = 1, shape = 2)

## Maximum likelihood estimator
(res <- MLEstimator(x = x, ParamFamily = G))

## Asymptotic (CLT-based) confidence interval
confint(res)

## implementation of ML-estimator of package MASS
library(MASS)
(res1 <- fitdistr(x, "gamma"))

## comparison
## shape
estimate(res)[2]
## rate
1/estimate(res)[1]
## log-likelihood
res1$loglik
## negative log-likelihood
criterion(res)
}
\keyword{univar}
