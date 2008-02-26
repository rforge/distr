\name{MLEstimator}
\alias{MLEstimator}

\title{ Function to compute maximum likelihood estimators }
\description{
  The function \code{MLEstimator} provides a general way to compute 
  maximum likelihood estimators for a given parametric family of 
  probability measures. This is done by calling the function 
  \code{MCEstimator} which minimizes the negative log-Likelihood.
}
\usage{
MLEstimator(x, ParamFamily, interval, par, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ (empirical) data }
  \item{ParamFamily}{ object of class \code{"ParamFamily"} }
  \item{interval}{ interval used by \code{optimize} }
  \item{par}{ initial parameter values used by \code{optim}; if \code{missing}
    the parameter of \code{ParamFamily} are used. }
  \item{\dots}{ further arguments to \code{criterion} or \code{optimize} 
    or \code{optim}, respectively. }
}
\details{
  The function calls \code{MCEstimator} using the negative log-likelihood
  as criterion which should be minimized.
}
\value{
  An object of S3-class \code{"MCEstimate"} which inherits from 
  class \code{"Estimate"}, a list with two components
  \item{estimate}{ maximum likelihood parameter estimate }
  \item{criterion}{ minimum value of the negative log-likelihood }
}
%\references{  }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link{ParamFamily-class}}, \code{\link{ParamFamily}}, code{\link{MCEstimator}},
\code{\link[MASS]{fitdistr}} }
\examples{
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
theta <- c(1, 2)
names(theta) <- c("scale", "shape")
G <- ParamFamily(name = "Gamma family",
        param = ParamFamParameter(name = "scale and shape", main = theta),
        distribution = Gammad(scale = 1, shape = 2),
        modifyParam = function(theta){ 
                          Gammad(scale = theta[1], shape = theta[2]) 
                      },
        props = paste(c("The Gamma family is scale invariant via the",
                  "parametrization '(nu,shape)=(log(scale),shape)'"),
                  collapse = " "))

## Maximum likelihood estimator
(res <- MLEstimator(x = x, ParamFamily = G))

## implementation of ML-estimator of package MASS
library(MASS)
(res1 <- fitdistr(x, "gamma"))

## comparison
## shape
res$estimate[2]
## rate
1/res$estimate[1]
## log-likelihood
res1$loglik
## negative log-likelihood
res$criterion
}
\keyword{univar}
