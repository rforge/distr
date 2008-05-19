\name{MCEstimator}
\alias{MCEstimator}

\title{ Function to compute minimum criterion estimators }
\description{
  The function \code{MCEstimator} provides a general way to compute 
  estimators for a given parametric family of probability measures which 
  can be obtain by minimizing a certain criterion. For instance, 
  the negative log-Likelihood in case of the maximum likelihood
  estimator or some distance between distributions like in 
  case of minimum distance estimators.
}
\usage{
MCEstimator(x, ParamFamily, criterion, crit.name, interval, par, ...)
}
\arguments{
  \item{x}{ (empirical) data }
  \item{ParamFamily}{ object of class \code{"ParamFamily"} }
  \item{criterion}{ function: criterion to minimize; see Details section. }
  \item{crit.name}{ optional name for criterion. }
  \item{interval}{ interval used by \code{optimize} }
  \item{par}{ initial parameter values used by \code{optim}; if \code{missing}
    the parameter of \code{ParamFamily} are used. }
  \item{\dots}{ further arguments to \code{criterion} or \code{optimize} 
    or \code{optim}, respectively. }
}
\details{
  The argument \code{criterion} has to be a function with arguments the 
  empirical data as well as an object of class \code{"Distribution"} 
  and possibly \code{\dots}.
}
\value{
  An object of S3-class \code{"MCEstimate"} which inherits from class \code{"Estimate"}, 
  a list with two components
  \item{estimate}{ minimum criterion parameter estimate }
  \item{criterion}{ minimum value of the criterion }
}
%\references{  }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link{ParamFamily-class}}, \code{\link{ParamFamily}} }
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

## Maximum Likelihood estimator
## Note: you can directly use function MLEstimator!
negLoglikelihood <- function(x, Distribution){
    res <- -sum(log(Distribution@d(x)))
    names(res) <- "Negative Log-Likelihood"
    return(res)
}
MCEstimator(x = x, ParamFamily = G, criterion = negLoglikelihood)

## Kolmogorov(-Smirnov) minimum distance estimator
## Note: you can also use function MDEstimator!
MCEstimator(x = x, ParamFamily = G, criterion = KolmogorovDist, 
            crit.name = "Kolmogorov distance")

## Total variation minimum distance estimator
## Note: you can also use function MDEstimator!
## discretize Gamma distribution
MCEstimator(x = x, ParamFamily = G, criterion = TotalVarDist, 
            crit.name = "Total variation distance")

## or smooth empirical distribution (takes some time!)
#MCEstimator(x = x, ParamFamily = G, criterion = TotalVarDist, 
#            asis.smooth.discretize = "smooth", crit.name = "Total variation distance")

## Hellinger minimum distance estimator
## Note: you can also use function MDEstimator!
## discretize Gamma distribution
MCEstimator(x = x, ParamFamily = G, criterion = HellingerDist, 
            crit.name = "Hellinger Distance")

## or smooth empirical distribution (takes some time!)
#MCEstimator(x = x, ParamFamily = G, criterion = HellingerDist, 
#            asis.smooth.discretize = "smooth", crit.name = "Hellinger distance")
}
\keyword{univar}
