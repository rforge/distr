\name{internal_mdehelpers_for_distrMod}
\alias{internal_mdehelpers_for_distrMod}
\alias{.CvMMDCovariance}

\title{Internal helper functions for treating MDEstimators in package distrMod}

\description{
These functions are used internally by function \code{MDEstimator} in 
package ``distrMod''.}

\usage{
.CvMMDCovariance<- function(L2Fam, param, mu = distribution(L2Fam),  
                            withplot = FALSE, withpreIC = FALSE,
                            N = getdistrOption("DefaultNrGridPoints")+1,
                            rel.tol=.Machine$double.eps^0.3, 
                            TruncQuantile = getdistrOption("TruncQuantile"), 
                            IQR.fac = 15, 
                            ...)
}


\arguments{
  \item{L2Fam}{an object of class \code{L2ParamFamily}; 
              the parametric family at which to evaluate the MCE}
  \item{param}{an object of class \code{ParamFamParameter}; 
               contains the parameter value at which to evaluate the 
			   variance}
  \item{mu}{an object of class \code{UnivariateDistribution}; 
            the distribution on the reals at which to integrate the squared
            distance of the cdf's in the CvM-distance}
  \item{withplot}{logical; defaults to \code{FALSE}; if \code{TRUE} for diagnostic  
                  purposes plots the influence function of the CvM-MDE}
  \item{withpreIC}{logical; should corresponding IC of the CvM-MDE be returned?}
  \item{\dots}{currently not used} 
  \item{N}{integer; the number of grid points at which to evaluate the antiderivative
          in case of an absolutely continuous distribution; more precisely, internally
		  this becomes \eqn{2N+1}}
  \item{rel.tol}{numeric; relative tolerance; currently not used}
  \item{TruncQuantile}{numeric in (0,1); in case of an unbounded support of the 
                       distribution the quantile level at which to cut the distribution}
  \item{IQR.fac}{a positive numeric; a factor by which to multiply the IQR of the distribution
                 to obtain a sensible cut of point for the integration bounds}
 }

\details{
\code{.CvMMDCovariance} computes the asymptotic covariance of the CvM-MDE according to
H. Rieder (1994) "Robust Asymptotic Statistics".
 }

\value{
\item{.CvMMDCovariance}{if argument \code{withpreIC} is \code{TRUE}, then
a list with elements the IC of the CvM-MDE and its covariance is returned, otherwise
just the covariance}
}

\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}
}

\keyword{internal}
\concept{utilities}
