\name{PrognCondDistribution}
\alias{PrognCondDistribution}

\title{Generating function for PrognCondDistribution-class}
\description{
  Generates an object of class \code{"PrognCondDistribution"}.
}
\usage{
PrognCondDistribution(Regr, Error,
             rel.tol= getdistrExOption("ErelativeTolerance"), 
             lowerTruncQuantile = getdistrExOption("ElowerTruncQuantile"), 
             upperTruncQuantile = getdistrExOption("EupperTruncQuantile"), 
             IQR.fac = getdistrExOption("IQR.fac"))
             
}
\arguments{
  \item{Regr}{object of class \code{AbscontDistribution};
  the distribution of \code{X}. }
  \item{Error}{object of class \code{AbscontDistribution};
  the distribution of \code{eps}. }
  \item{rel.tol}{relative tolerance for \code{distrExIntegrate}.} 
  \item{lowerTruncQuantile}{lower quantile for quantile based integration range.}
  \item{upperTruncQuantile}{upper quantile for quantile based integration range.}
  \item{IQR.fac}{factor for scale based integration range (i.e.; 
  median of the distribution \eqn{\pm}{+-}\code{IQR.fac}\eqn{\times}{*}IQR).}
}
\details{For independent r.v.'s X,E with univariate, absolutely continuous 
 (a.c.) distributions \code{Regr} and \code{Error}, respectively, 
  \code{PrognCondDistribution()} returns the 
  (factorized, conditional) posterior distribution of X given X+E=y.
  as an object of class \code{PrognCondDistribution}.
}
\value{Object of class \code{"PrognCondDistribution"}}
%\references{}
\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},
}
%\note{}
\seealso{\code{PrognCondDistribution-class}; demo(\file{Prognose.R}).}
\examples{
PrognCondDistribution(Error = ConvexContamination(Norm(), Norm(4,1), size=0.1))
}
\keyword{distribution}
\concept{conditional distribution}
\concept{S4 distribution class}
\concept{generating function}
