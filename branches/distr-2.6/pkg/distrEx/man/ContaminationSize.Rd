\name{ContaminationSize}
\alias{ContaminationSize}
\alias{ContaminationSize-methods}
\alias{ContaminationSize,AbscontDistribution,AbscontDistribution-method}
\alias{ContaminationSize,DiscreteDistribution,DiscreteDistribution-method}
\alias{ContaminationSize,LatticeDistribution,DiscreteDistribution-method}
\alias{ContaminationSize,DiscreteDistribution,LatticeDistribution-method}
\alias{ContaminationSize,LatticeDistribution,LatticeDistribution-method}
\alias{ContaminationSize,AcDcLcDistribution,AcDcLcDistribution-method}

\title{Generic Function for the Computation of the Convex Contamination 
    (Pseudo-)Distance of Two Distributions}
\description{
  Generic function for the computation of convex contamination (pseudo-)distance 
  of two probability distributions \eqn{P} and \eqn{Q}. That is, the 
  minimal size \eqn{\varepsilon\in [0,1]}{0 <= epsilon <= 1} is computed such that 
  there exists some probability distribution \eqn{R} with
  \deqn{Q = (1-\varepsilon)P + \varepsilon R}{Q = (1 - epsilon)P + epsilon R}
}
\usage{
ContaminationSize(e1, e2, ...)
\S4method{ContaminationSize}{AbscontDistribution,AbscontDistribution}(e1,e2)
\S4method{ContaminationSize}{DiscreteDistribution,DiscreteDistribution}(e1,e2)
\S4method{ContaminationSize}{AcDcLcDistribution,AcDcLcDistribution}(e1,e2)
}
\arguments{
  \item{e1}{ object of class \code{"Distribution"} }
  \item{e2}{ object of class \code{"Distribution"} }
  \item{\dots}{ further arguments to be used in particular methods (not in package \pkg{distrEx})}
}
\details{Computes the distance from \code{e1} to \code{e2} respectively
  \eqn{P} to \eqn{Q}. This is not really a distance as it is not symmetric!}
\value{
  A list containing the following components:
  \item{e1}{ object of class \code{"Distribution"}; ideal distribution  }
  \item{e2}{ object of class \code{"Distribution"}; 'contaminated' distribution }
  \item{size.of.contamination}{ size of contamination }
}
\section{Methods}{
\describe{
  \item{e1 = "AbscontDistribution", e2 = "AbscontDistribution":}{
    convex contamination (pseudo-)distance of two absolutely
    continuous univariate distributions.
  }
  \item{e1 = "DiscreteDistribution", e2 = "DiscreteDistribution":}{
    convex contamination (pseudo-)distance of two discrete 
    univariate distributions.
  }
  \item{e1 = "AcDcLcDistribution", e2 = "AcDcLcDistribution":}{
    convex contamination (pseudo-)distance of two discrete 
    univariate distributions.
  }
  }
}
\references{Huber, P.J. (1981) \emph{Robust Statistics}. New York: Wiley.}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{KolmogorovDist}}, \code{\link{TotalVarDist}}, 
    \code{\link{HellingerDist}}, \code{\link[distr]{Distribution-class}}}
\examples{
ContaminationSize(Norm(), Norm(mean=0.1))
ContaminationSize(Pois(), Pois(1.5))
}
\concept{convex contamination}
\concept{gross error}
\keyword{distribution}
\concept{distance}
