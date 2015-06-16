\name{plotCLT}
\alias{plotCLT}
\alias{plotCLT-methods}
\alias{plotCLT,AbscontDistribution-method}
\alias{plotCLT,DiscreteDistribution-method}

\title{Generic Plot Function for Illustrating the CLT}
\description{Generic 'plot' function for generating the plots of 'illustrateCLT'.}
%
\usage{plotCLT(Tn, ...)
\S4method{plotCLT}{AbscontDistribution}(Tn, k, summands="")
\S4method{plotCLT}{DiscreteDistribution}(Tn, k, summands="")
}
\arguments{
  \item{Tn}{object of class \code{"AbscontDistribution"} or class
            \code{"DiscreteDistribution"}: distribution of the summands}
  \item{k}{integer: number off summands to be plotted as graphics title}
  \item{summands}{character: name of the summands}
  \item{\dots}{addtional arguments for methods not yet implemented }
}
%\details{}
\value{void}

\seealso{
\code{\link{illustrateCLT}}
}

\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}\cr
Matthias Kohl \email{Matthias.Kohl@stamats.de} }
\examples{
illustrateCLT(Distr = Unif(), len = 20)
}
\keyword{internal}
\keyword{methods}
\keyword{hplot}
\keyword{distribution}
